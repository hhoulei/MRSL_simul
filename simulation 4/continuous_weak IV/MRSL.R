
Rcpp::sourceCpp('STEP1.cpp')
Rcpp::sourceCpp('STEP2.cpp')
Rcpp::sourceCpp('DFS.cpp')

MRSL <- function(data_sum_beta,data_sum_se,beta,cutoff,
                 adj_methods,method_step1,vary_mvmr_adj,N,R){
  
  amatt0 <- list()
  
  if(method_step1=='ivw'){
    res1 <- STEP1(data_sum_beta,data_sum_se,beta,use_eggers_step1=0)
    amatt0[[1]] <- res1$amatt
  }else if(method_step1=='egger'){
    res1 <- STEP1(data_sum_beta,data_sum_se,beta,use_eggers_step1=1)
    amatt0[[1]] <- res1$amatt
  }else{
    res1 <- STEP1_pleio(data_sum_beta,data_sum_se,N,method_step1,beta)
    amatt0[[1]] <- res1
  }
  
  
 
  pp <- amatt0[[1]]
  v1 <- 7-c(DFS(amattt=pp,n_nodes=ncol(pp)))
  v2 <- 1:ncol(data_sum_beta)

  
  SpearF <- sum(sapply(seq_along(v1), function(i) abs(i - (which(v2 == v1[i])))))
  
  kenDcor <- cor.test(v1,v2, use = "pairwise", method="kendall") 
  kenDcor_res <- c(kenDcor$statistic,kenDcor$p.value,kenDcor$estimate)
  
  
  amatt0[[1]] <- amatt0[[1]][v1,v1]
  
  R1 <- R[v1,v1]
  
  just <- TRUE
  i=1
  while(just){
    cat("iteration--",i)
    i=i+1
    amatt055 <- STEP2(amatt0[[i-1]],data_sum_beta,data_sum_se,beta,
                  adj_methods,use_eggers_step2=0,vary_mvmr_adj)
    amatt0[[i]] <- amatt055$amatt
    just <- !all(amatt0[[i]]==amatt0[[i-1]])
  }
  
  return(list(#amatt=amatt0[[i]],
              amatt=amatt055$amatt,
              amatt_step1=pp,
              iteration=i,
              SpearF=SpearF,
              kenDcor_res=kenDcor_res,
              R1=R1))
}