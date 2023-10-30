

# a faster version of the above
#' Remove non-minimal separating sets.
#' 
#' @param sepsets a list of sets
#' @return a subset of the input lits
#' @details remove all sets such that there exists another set that is contained in them; useful for cleaning instrument sets
remove_non_minimal_sepsets<-function(l){
  if(length(l)==0){return(list())}
  if(length(l)==1){return(l)}
  
  sizes = sapply(l,length)
  l = l[order(sizes)]
  sizes = sapply(l,length)
  i = 1
  while(i < length(l)){
    set1 = l[[i]]
    to_rem = rep(F,length(l))
    for(j in (i+1):length(l)){
      if(sizes[i]==sizes[j]){next}
      set2 = l[[j]]
      if(all(set1 %in% set2)){
        to_rem[j] = T
      }
    }
    i = i+1
    l = l[!to_rem]
    sizes = sizes[!to_rem]
  }
  
  return(l)
}


# Functions for simulating data and obtaining the cGAUGR/MR results 
# Get the effect size, se, and p-value for x~y|z 
run_lm<-function(x,y,z,df){
  if(is.null(z)){
    df = data.frame(x=df[,x],y=df[,y])
  }
  else{
    df = data.frame(x=df[,x],y=df[,y],df[,z])
  }
  model = lm(x~.,data=df)
  coefs = summary(model)$coefficients
  return(coefs[2,])
}
# get distances (ancestry graph)
igraph_directed_distances<-function(Bg){
  distances = igraph::distances(Bg) # undirected distances
  for(i in 1:nrow(distances)){
    for(j in 1:ncol(distances)){
      distances[i,j] = length(igraph::shortest_paths(Bg,from=j,to=i)$vpath[[1]])-1
    }
  }
  return(distances)
}
# a function to add the known distances to the results
add_distances<-function(m,dists,newcolname = "KnownDistance"){
  m[,1] = as.character(m[,1])
  m[,2] = as.character(m[,2])
  v = c()
  for(i in 1:nrow(m)){
    v = c(v,dists[m[i,2],m[i,1]]) # add dist from exposure to outcome
  }
  m = cbind(m,v)
  colnames(m)[ncol(m)] = newcolname
  return(m)
}

extract_skeleton_G_VT<-function(GWAS_Ps,trait_pair_pvals,P1,P2,test_columns = NULL){
  G_VT = GWAS_Ps <= P1
  num_ivs = nrow(GWAS_Ps)
  num_traits = ncol(GWAS_Ps)
  iv_trait_sepsets = list()
  for(i in 1:num_traits){
    tr1 = colnames(GWAS_Ps)[i]
    print(paste("#####",tr1))
    for(j in 1:num_traits){
      if(i==j){next}
      tr2 = colnames(GWAS_Ps)[j]
      curr_p_mat = trait_pair_pvals[[tr1]][[tr2]][rownames(GWAS_Ps),]
      curr_columns = test_columns
      if(is.null(curr_columns)){
        curr_columns = colnames(curr_p_mat)
      }
      if(is.character(curr_columns)){
        curr_columns = intersect(curr_columns,colnames(curr_p_mat))
      }
      for (testname in curr_columns){
        # Do we have significant SNPs that become non-sig?
        curr_ci_test_results = curr_p_mat[,testname] >= P2
        curr_ci_test_results[is.na(curr_ci_test_results)]=F
        curr_sep_snps = rownames(curr_p_mat)[curr_ci_test_results]
        curr_new_sep_snps = curr_sep_snps[G_VT[curr_sep_snps,tr1]]
        G_VT[curr_sep_snps,tr1] = F
        num_new_seps = length(curr_new_sep_snps)
        if(num_new_seps>0){print(paste(num_new_seps,
                                       "new sepsets were found for tr2:",tr2))}
        curr_sepset = tr2
        if(num_new_seps > 0){
          if(is.null(iv_trait_sepsets[[tr1]])){iv_trait_sepsets[[tr1]] = list()}
          for(snp in curr_new_sep_snps){
            iv_trait_sepsets[[tr1]][[snp]] = union(iv_trait_sepsets[[tr1]][[snp]],curr_sepset)
          }
        }
      }
    }
  }
  return(list(G_VT,iv_trait_sepsets))
}

run_pairwise_mr_analyses_with_iv_sets<-function(sum_stats,sum_stats_se,iv_sets,
                                                minIVs = 3,...){
  trait_pairs_analysis = c()
  traits = colnames(sum_stats)
  num_tests = 0
  for(tr1 in traits){
    for(tr2 in traits){
      if(tr1==tr2){next}
      ivs = iv_sets[[tr1]][[tr2]]
      if(length(ivs)<minIVs){next}
      try({ # required as some MR methods may fail
        curr_mr_res = run_single_mr_analysis(ivs,tr1,tr2,sum_stats,sum_stats_se,...);
        trait_pairs_analysis = rbind(trait_pairs_analysis,c(tr1,tr2,curr_mr_res,length(ivs)))        
      })
    }
  }
  if(!is.null(dim(trait_pairs_analysis))){
    colnames(trait_pairs_analysis) = c("Exposure","Outcome","p","p_het","est","Q","NumIVs")
    trait_pairs_analysis = as.data.frame(trait_pairs_analysis)
    for(j in 3:ncol(trait_pairs_analysis)){
      trait_pairs_analysis[[j]] = as.numeric(as.character(trait_pairs_analysis[[j]]))
    }
  }
  return(trait_pairs_analysis)
}

run_single_mr_analysis<-function(snpset,tr1,tr2,X,Y,func=mr_egger,...){
  mr_in = mr_input(X[snpset,tr1],Y[snpset,tr1],X[snpset,tr2],Y[snpset,tr2])
  xx = func(mr_in,...)
  p = 1
  if(is.element("Pvalue.Est",set=slotNames(xx))){p=xx@Pvalue.Est}
  if(is.element("Pvalue",set=slotNames(xx))){p=xx@Pvalue}
  p_het = 1;Q=0;I2=100
  if(is.element("Heter.Stat",set=slotNames(xx))){
    p_het = xx@Heter.Stat[2]
    Q = xx@Heter.Stat[1]
  }
  est = 0
  if(is.element("Estimate",set=slotNames(xx))){est = xx@Estimate}
  return(c(p,p_het,est,Q))
}




cGAUGE_algorithm <- function(data_ind,D,R,beta,mmethod){
  
  
  p1=0.001
  p2=0.01
  cgaugeMode=1
  
  simulated_data <- as.data.frame(data_ind)
  
  p = D;
  num_ivs = ncol(data_ind)- p;
  phenos = paste("T",1:p,sep="")
  ivs = paste("IV",1:num_ivs,sep="")
  
  colnames(simulated_data)[1:p] = phenos
  colnames(simulated_data)[(p+1):ncol(simulated_data)] = ivs
  
  
  B=R
  
  Bg = igraph::graph_from_adjacency_matrix(t(abs(B)>0)[1:p,1:p])
  # plot(igraph::simplify(Bg))
  # is.dag(Bg)
  # print("Completed creating the traits graph")
  # print("In-degrees:")
  # print(rowSums(abs(B)>0))
  # print("Out-degrees:")
  # print(colSums(abs(B)>0))
  # print("Is DAG?")
  # print(is_dag(Bg))
  
  # Compute distances in B, among phenotypes
  B_distances = igraph_directed_distances(Bg)
  colnames(B_distances) = phenos
  rownames(B_distances) = phenos
  
  
  B <- rbind(B,beta)
  for(oor in 1:num_ivs){
    B <- cbind(B,rep(0,num_ivs+p))
  }
  colnames(B) <- rownames(B) <- colnames(simulated_data)
  

  df = data.frame(simulated_data)
  # Get all IV-phenotype associations
  GWAS_Ps = matrix(1,num_ivs,p,dimnames = list(ivs,phenos))
  GWAS_effects = matrix(0,num_ivs,p,dimnames = list(ivs,phenos))
  GWAS_ses = matrix(0,num_ivs,p,dimnames = list(ivs,phenos))
  GWAS_Zs = matrix(0,num_ivs,p,dimnames = list(ivs,phenos))
  for(pheno in phenos){
    # print(pheno)
    gwas_res = sapply(ivs,run_lm,x=pheno,z=NULL,df = df)
    GWAS_Ps[,pheno] = gwas_res[4,]
    GWAS_effects[,pheno] = gwas_res[1,]
    GWAS_ses[,pheno] = gwas_res[2,]
    GWAS_Zs[,pheno] = gwas_res[3,]
  }
  
  G_it = GWAS_Ps < p1
  
  # print("Starting the cGAUGE CI analysis")
  # Skeleton learning
  # G_t
  # print("Computing the trait skeleton matrix")
  p_thr = p1
  skeleton_pmax = matrix(-1,p,p,dimnames=list(phenos,phenos))
  sepsets = list()
  for(tr1 in phenos){
    sepsets[[tr1]] = list()
    for(tr2 in phenos){
      sepsets[[tr1]][[tr2]] = list()
    }
  }
  
  
  # Go over singletons
  for(tr1 in phenos){
    for(tr2 in phenos){
      if(tr1==tr2){break}
      skeleton_pmax[tr1,tr2] = run_lm(tr1,tr2,NULL,simulated_data)[4]
      if(skeleton_pmax[tr1,tr2]>p_thr){
        skeleton_pmax[tr2,tr1] = skeleton_pmax[tr1,tr2]
        next
      }
      # go over singletons
      for(tr3 in phenos){
        if (tr3 %in% c(tr1,tr2)){next}
        currp = run_lm(tr1,tr2,tr3,simulated_data)[4]
        skeleton_pmax[tr1,tr2] = max(skeleton_pmax[tr1,tr2],currp)
        if(currp > p1){sepsets[[tr1]][[tr2]][[tr3]] = list(p=currp,sep=tr3)}
      }
      sepsets[[tr2]][[tr1]] = sepsets[[tr1]][[tr2]]
      skeleton_pmax[tr2,tr1] = skeleton_pmax[tr1,tr2]
    }
  }
  # Go over pairs
  for(tr1 in phenos){
    for(tr2 in phenos){
      if(tr1==tr2){break}
      if(skeleton_pmax[tr1,tr2]>p_thr){next}
      for(tr3 in phenos){
        if (tr3 %in% c(tr1,tr2)){next}
        for(tr4 in phenos){
          if (tr4 %in% c(tr1,tr2,tr3)){next}
          currp = run_lm(tr1,tr2,c(tr3,tr4),simulated_data)[4]
          skeleton_pmax[tr1,tr2] = max(skeleton_pmax[tr1,tr2],currp)
          if(currp > p1){
            sepsets[[tr1]][[tr2]][[paste(tr1,tr2,sep=";")]] = 
              list(p=currp,sep=c(tr4,tr3))
          }
        }
      }
      sepsets[[tr2]][[tr1]] = sepsets[[tr1]][[tr2]]
      skeleton_pmax[tr2,tr1] = skeleton_pmax[tr1,tr2]
    }
  }
  G_t = skeleton_pmax < p1
  # print("Done, node degrees:")
  # print(colSums(G_t))
  
  
  # Merge and clean the sepsets
  merged_sepsets = list()
  for(tr1 in phenos){
    merged_sepsets[[tr1]] = list()
    for(tr2 in phenos){
      l = sepsets[[tr1]][[tr2]]
      l = l[sapply(l,function(x)x$p)>p1]
      l = lapply(l,function(x)x$sep)
      if(length(l)==0){next}
      reduced_l = remove_non_minimal_sepsets(l)
      # print(paste(length(l),length(reduced_l)))
      merged_sepsets[[tr1]][[tr2]] = unique(unlist(reduced_l))
    }
  }
  
  # G_vt
  # print("Computing all instrument vs trait pair CI tests:")
  trait_pair_pvals = list()
  for(pheno1 in phenos){
    trait_pair_pvals[[pheno1]] = list()
    for(pheno2 in phenos){
      if(pheno1==pheno2){next}
      gwas_res = t(sapply(ivs,run_lm,x=pheno1,z=pheno2,df = df))
      gwas_res = gwas_res[,4:1]
      trait_pair_pvals[[pheno1]][[pheno2]] = gwas_res
    }
  }
  G_vt = extract_skeleton_G_VT(GWAS_Ps,trait_pair_pvals,P1=p1,P2=p2,test_columns = 1)[[1]]
  real_G_vt = abs(t(B[phenos,ivs])>0)
  
  
  
  # Get new instrument sets after the cGAUGE filter
  iv_sets = list()

  if(cgaugeMode == "1"){
    uniquely_mapped_ivs = rownames(G_vt)[rowSums(G_vt)==1]
    for(tr1 in phenos){
      iv_sets[[tr1]] = list()
      for(tr2 in phenos){
        iv_sets[[tr1]][[tr2]] = intersect(rownames(G_vt)[G_vt[,tr1]>0],uniquely_mapped_ivs)
        # iv_sets[[tr1]][[tr2]] = rownames(GWAS_Ps)[GWAS_Ps[,tr1]<p1]
        # iv_sets[[tr1]][[tr2]] = intersect(iv_sets[[tr1]][[tr2]],uniquely_mapped_ivs)
      }
    }
  }
  
  # Run the MR
  # print("Done, rerunning MR")
  # Pleio size is set to 1, to satisfy the conditions of Theorem 3.1
  
  if(mmethod=="ivw"){
    cgauge_mr_anal_res = list(
      "IVW" = run_pairwise_mr_analyses_with_iv_sets(GWAS_effects,GWAS_ses,iv_sets,
                                                    func=mr_ivw,robust=T)
    )
    
    cgauge_ivw_res = cgauge_mr_anal_res$IVW
    if(!is.null(dim(cgauge_mr_anal_res$IVW))){
      cgauge_ivw_res = add_distances(cgauge_ivw_res,B_distances)
      cgauge_ivw_res = add_distances(cgauge_ivw_res,1-G_t,newcolname = "PleioProperty")
    }
    
    amatt_ivw = matrix(0,p,p)
    colnames(amatt_ivw) <- rownames(amatt_ivw) <- phenos
    
    cgauge_ivw_res <- cgauge_ivw_res[cgauge_ivw_res$p<0.05,]
    amatt_ivw[cgauge_ivw_res$Exposure,cgauge_ivw_res$Outcome] = 1
    
    return(list(amatt=amatt_ivw))
    
  }else if(mmethod=="egger"){
    
    cgauge_mr_anal_res = list(
      "Egger" = run_pairwise_mr_analyses_with_iv_sets(GWAS_effects,GWAS_ses,iv_sets,
                                                      func=mr_egger,robust=T)
    )
    # Add the known distances
    cgauge_egger_res = cgauge_mr_anal_res$Egger
    if(!is.null(dim(cgauge_mr_anal_res$Egger))){
      cgauge_egger_res = add_distances(cgauge_mr_anal_res$Egger,B_distances)
      cgauge_egger_res = add_distances(cgauge_egger_res,1-G_t,newcolname = "PleioProperty")
    }
    

    amatt_egger = matrix(0,p,p)
    colnames(amatt_egger) <- rownames(amatt_egger) <- phenos
    
    cgauge_egger_res <- cgauge_egger_res[cgauge_egger_res$p<0.05,]
    amatt_egger[cgauge_egger_res$Exposure,cgauge_egger_res$Outcome] = 1
    
    return(list(amattr=amatt_egger))
    
  }else if(mmethod=="mrpresso"){
    
    print(" MRPRESSO")
    cgauge_mrpresso_res = c()
    try({
      # Add MRPRESSO
      for(tr1 in phenos){
        for(tr2 in phenos){
          if(tr1==tr2){next}
          currivs = iv_sets[[tr1]][[tr2]]
          if(length(currivs)<5){next}
          X = data.frame(E1b=GWAS_effects[currivs,tr1],O1b=GWAS_effects[currivs,tr2],
                         E1sd=GWAS_ses[currivs,tr1],O1sd=GWAS_ses[currivs,tr2])
          try({
            res = mr_presso(BetaOutcome = "O1b", BetaExposure = "E1b", 
                            SdOutcome = "O1sd", SdExposure = "E1sd",data=X,
                            OUTLIERtest=T,
                            DISTORTIONtest = T,
                            NbDistribution = 100,SignifThreshold = 0.1)
            if(is.na(res$`Main MR results`[2,"P-value"])){
              cgauge_mrpresso_res = rbind(cgauge_mrpresso_res,
                                          c(tr1,tr2,unlist(res$`Main MR results`[1,])))
            }
            else{
              cgauge_mrpresso_res = rbind(cgauge_mrpresso_res,
                                          c(tr1,tr2,unlist(res$`Main MR results`[2,])))
            }
          })
        }
      }
      if(!is.null(dim(cgauge_mrpresso_res))){
        cgauge_mrpresso_res = as.data.frame(cgauge_mrpresso_res)
        for(j in 3:ncol(cgauge_mrpresso_res)){
          cgauge_mrpresso_res[[j]] = as.numeric(as.character(cgauge_mrpresso_res[[j]]))
        }
      }
      # Add distances and the nonpleio property
      cgauge_mrpresso_res = add_distances(cgauge_mrpresso_res,B_distances)
      cgauge_mrpresso_res = add_distances(cgauge_mrpresso_res,1-G_t,"PleioProperty")
      
    })
    
    amatt_mrpresso = matrix(0,p,p)
    colnames(amatt_mrpresso) <- rownames(amatt_mrpresso) <- phenos
    
    cgauge_mrpresso_res <- cgauge_mrpresso_res[cgauge_mrpresso_res$`P-value`<0.05,]
    amatt_mrpresso[cgauge_mrpresso_res$V1,cgauge_mrpresso_res$V2] = 1
    
    return(list(amatt_mrpresso=amatt_mrpresso))
  }
  
}







Open_path_adj <- function(MM,start0,end0,adj){

  locname <- which(MM!=0,arr.ind = T)
  dagstatement <- "dag {"
  for(i in 1:nrow(locname)){
    dagstatement <- paste0(dagstatement,
                           "D",locname[i,1]," -> ","D",locname[i,2]," ; ")
  }
  dagstatement <- substr(dagstatement,1,nchar(dagstatement)-3)
  dagstatement <- paste0(dagstatement,"}")

  g <- dagitty(dagstatement)

  start0=paste0("D",start0)
  end0=paste0("D",end0)
  
  if(adj==2){
    
    node_adj <- NULL
    
    pp <- paths( g, start0, end0, directed=F ) ##list the paths
    if(length(pp)!=0){
      openpath <- pp$paths[pp$open]
      nodes_all <- strsplit(openpath," ")

      if(length(nodes_all)!=0) {
        for(oi in 1:length(nodes_all)){
          #cat("ppR2_",oi,'\n')
          node_once <- nodes_all[[oi]]
          #cat("ppR3",'\n')
          if(length(node_once)>3){
            node_loc <- seq(3,length(node_once)-2,2)
            node_adj <- c(node_adj,node_once[node_loc])
          }
        }
      }

    }

    
  }else if(adj==3){
    
    loc_adj0 <- adjustmentSets(g,start0,end0)
  
    if(length(loc_adj0)==0){
      node_adj <- NULL
    }else{
      node_adj <- loc_adj0[[1]]
    }
    

    pp2=paths( g, start0, end0, directed=TRUE )$paths

    if(length(pp2)!=0){
      
      nodes_all <- strsplit(pp2," ")
      
      for(oi in 1:length(nodes_all)){
        node_once <- nodes_all[[oi]]
        if(length(node_once)>3){
          node_loc <- seq(3,length(node_once)-2,2)
          node_adj <- c(node_adj,node_once[node_loc])
        }
      }
      
    }


  }


  if(length(node_adj)!=0){
    node_adj <- unique(node_adj)
    node_adj <- as.numeric(substr(node_adj,2,nchar(node_adj)))
    node_adj <- node_adj-1
  }
  
  node_adj <- as.numeric(node_adj)

  return(node_adj)
}


Method_MRPC <- function(data,g,method){
  
  n <- nrow (data) # Number of rows
  V <- colnames(data) # Column names
  suffStat_C <- list(C = cor(data),
                     n = n)
  
 #  if(method=="gaussCItest"){
    MRPC.fit <- MRPC(data,
                     suffStat = suffStat_C,
                     GV = g,
                     FDR = 0.05,
                     indepTest = 'gaussCItest',
                     labels = V,
                     FDRcontrol = TRUE,
                     verbose = FALSE)
  # }else if(method=="disCItest"){
  #   MRPC.fit <- MRPC(data,
  #                    #suffStat = suffStat_C,
  #                    GV = g,
  #                    FDR = 0.05,
  #                    indepTest = 'disCItest',
  #                    labels = V,
  #                    FDRcontrol = TRUE,
  #                    verbose = TRUE)
  # }



  return(MRPC.fit)
}


Method_HC <- function(dt_MRPC,D){
  
  # whitelist <- data.frame(
  #   from = paste0("SNP",1:D),
  #   to = paste0("D",1:D)
  # )
  
  blacklist <- data.frame(
    from = c(rep(paste0("D",1:D),each=D),
             rep(paste0("SNP",1:D),each=D)),
    to = rep(rep(paste0("SNP",1:D),D),2)
  )
  
  # ccom <- rbind(whitelist,blacklist)
  # blacklist <- ccom[!duplicated(ccom),] 
  # blacklist <- blacklist[-c(1:D),]
  
  hc.fit <- hc(dt_MRPC,start = NULL,whitelist = NULL,blacklist)
  rere <- hc.fit$arcs
  
  x.inv1 <- try(rere[,1], silent=TRUE)
  if ('try-error' %in% class(x.inv1)){
    amatt <- matrix(0,nrow = D,ncol=D)
  } else{
    loss <- !(rere[,1] %in% paste0("SNP",1:D))
    
    if(sum(loss)==1){
      rere <- rere[loss,]
      amatt <- matrix(0,nrow = D,ncol=D)
      colnames(amatt) <- rownames(amatt) <- paste0("D",1:D)
      amatt[rere[1],rere[2]] <- 1
    }else{
      rere <- rere[loss,]
      amatt <- matrix(0,nrow = D,ncol=D)
      colnames(amatt) <- rownames(amatt) <- paste0("D",1:D)
      amatt[rere[,1],rere[,2]] <- 1
    }
    

  }
  
  
  return(amatt)
}



BIMMER_Algorithm <- function(beta,data_sum_beta,data_sum_se,D){
  
  data_sum_beta <- as.data.frame(data_sum_beta)
  data_sum_se <- as.data.frame(data_sum_se)
  sumstats_1 <- list(beta_hat=data_sum_beta,
                     se_hat=data_sum_se)

  pheno <- paste0("D",1:D)
  snpid <- paste0("rs",1:nrow(data_sum_beta))
  
  colnames(sumstats_1$beta_hat) <- pheno
  colnames(sumstats_1$se_hat) <- pheno
  rownames(sumstats_1$beta_hat) <- snpid
  rownames(sumstats_1$se_hat) <- snpid

  snps_to_use <- list()
  for(i in 1:D){
    snps_to_use[[pheno[i]]] <- snpid[which(beta[,i]!=0)]
  }
  names(snps_to_use) <- pheno
  
  ## selected_snps <- bimmer::select_snps(sumstats = sumstats_1)
  selected_snps <- list()
  for(i in 1:length(pheno)){

    names <- snps_to_use[[i]]
    selected_snps_once <- list(names=names)
    for(j in 1:length(pheno)){
      selected_snps_once[[j+1]] <- sumstats_1$beta_hat[names,pheno[j]]
    }
    names(selected_snps_once) <- c("names",pheno)
    selected_snps[[i]] <- selected_snps_once
  }
  names(selected_snps) <- pheno
  
  tce_result <- bimmer::fit_tce(sumstats = sumstats_1, selected_snps = selected_snps)
  tce_filtered <- bimmer::filter_tce(tce_result$R_tce, tce_result$SE_tce)
  
  #weights <- inspre::make_weights(SE = tce_filtered$SE_tce, max_min_ratio = 10000)
  dce_result <- bimmer::fit_inspre(R_tce = tce_filtered$R_tce)
  
  selected_index <- which((dce_result$lambda-0.025)<0)[1]
  
  # if(is.null(dce_result$D_hat)){
  #   selected_index <- 1
  # }else{
  #   selected_index <- which.min(abs(dce_result$D_hat-0.025))[1] - 1
  # }

  lambda <- dce_result$lambda[selected_index]
  R_hat <- dce_result$R_hat[,,selected_index]
  U_hat <- dce_result$U[selected_index]
  
  ####
  amatt <- matrix(0,nrow=D,ncol=D)
  colnames(amatt) <- rownames(amatt) <- pheno
  
  na2 <- colnames(R_hat)
  na1 <- rownames(R_hat)
  for(t1 in 1:nrow(R_hat)){
    for(t2 in 1:ncol(R_hat)){
      if( R_hat[t1,t2] !=0) amatt[na1[t1],na2[t2]] <- 1
      
      
    }
  }
  
  return(list(amatt=amatt,lambda=lambda,R_hat=R_hat,U_hat=U_hat))
}


calc_metrics <- function(X, X_true, eps = 1e-10) {
  X <- as.matrix(X)
  X_true <- as.matrix(X_true)
  
  SHDD <- SHD(X, X_true)
  
  D <- ncol(X)
  al_N <- D*D
  
  TP <- sum(X!=0 & X_true!=0)
  FP <- sum(X!=0 & X_true==0)
  FN <- sum(X==0 & X_true!=0)
  TN <- sum(X==0 & X_true==0)
  
  precision = TP / (TP + FP)
  recall = TP / (TP + FN)
  TPR = TP / (TP + FN)
  TNR = TN / (TN + FP)
  acc = (TP + TN) / al_N
  F1 = 2*precision*recall/(precision + recall)
  FPR = 1-TNR

  return(list("TP"=TP,
              "FP"=FP,
              "FN"=FN,
              "TN"=TN,
              "precision" = precision, 
              "recall" = recall,
              "F1" = F1,
              "acc" = acc,
              "TPR" = TPR,
              "TNR" = TNR,
              "FPR" = FPR,
              "SHD" = SHDD))

}




