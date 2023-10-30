
library(bnlearn)
library(MRPC)
library(pcalg)
library(doMC)
library(doParallel)
#library(Rcpp2doParallel)
library(foreach)
library(dagitty)
library(bimmer)

library(Rcpp)
library(RcppArmadillo)
library(MRPRESSO)
library(MendelianRandomization)

source("DataGeneration_magic.R")
source("Algorithm.R")
source("MRSL.R")
source("Method_UVMR.R")
source("SHD.R")

Comp <- function(x,N,D_beta,each_g,gl,gu,dlB,duB,prob_corp,D_beta_M){
  
  D=7

  fdata <- DataGeneration_magic(N,each_g,gl,gu,dlB,duB,D_beta,prob_corp)
  R <- fdata$D_beta
  data_ind <- fdata$data_ind
  data_sum <- fdata$data_sum
  beta <- fdata$G_beta
  
  rr <- NULL
  ff <- NULL
  for(hh in 1:D){
    total_g <- which(beta[,hh]!=0)+D
    dd_G <- as.data.frame(data_ind[,total_g])
    fit <- lm(data_ind[,hh]~.,dd_G)
    ff <- c(ff,summary(fit)$fstatistic[1])
    rr <- c(rr,summary(fit)$r.squared)
  }
  
  data_sum_beta <- NULL
  data_sum_se <- NULL
  #data_sum_pval <- NULL
  for(oi in 1:length(data_sum)){
    data_sum_beta <- cbind(data_sum_beta,data_sum[[oi]][,1])
    data_sum_se <- cbind(data_sum_se,data_sum[[oi]][,2])
    #data_sum_pval <- cbind(data_sum_pval,data_sum[[oi]][,3])
  }


  #################### MRSL ##########################################

  #remove collier
  method_name <- c('weighted median','Mode based',
                   #'MR.Robust',
                   #'contamination mixture',
                   'MR.Mix',
                   'MR.RAPS',
                   #'mrcML.DP',
                   'CDcML.DP.S',
                   'CD.Egger'
                   #'CD.Ratio'
                   )
  method_name <- c('ivw','egger',method_name)
  
  res1 <- vector('list',length(method_name))
  names(res1) <- method_name
  for(mp in 1:length(method_name)){
    
    cat('MRSL=',mp,'\n')

    
    ptm <- Sys.time()
    x.inv1 <- try(MRSL(data_sum_beta,data_sum_se,beta,cutoff=0.05,
                       adj_methods=1,method_step1=method_name[mp],
                       vary_mvmr_adj=1,N,R), silent=TRUE)
   
    # adj_methods: 1 remove collier; 2 variables in open path; 3 min sufficient sets
    
    if ('try-error' %in% class(x.inv1)){
      res11 <- rep(NA,12)
      time11 <- NA
    } else{
      result1 <- x.inv1
      time11 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
      res11 <- unlist(calc_metrics(X=result1$amatt, X_true=result1$R1))
      res11_step1 <- unlist(calc_metrics(X=result1$amatt_step1, X_true=D_beta_M))
      order_met <- c(result1$SpearF,result1$kenDcor_res)
    }
    rm(x.inv1)
    res1[[mp]] <- list(time=time11,
                       res=res11,
                       order_met=order_met,
                       res11_step1=res11_step1,
                       amatt_step1=result1$amatt_step1)
    print(paste0("method 1 -  ",mp))
  }

  
  #################### MRPC ##########################################
  locdd <- NULL
  for(dd in 1:D){
    locdd <- c(locdd,which.min(data_sum[[dd]]$pval))
  }
  
  sig_SNP <- data_ind[,locdd+D]
  colnames(sig_SNP) <- paste0("SNP",1:D)
  dt_MRPC <- cbind(sig_SNP,data_ind[,1:D])
  
  dt_score <- fdata$score
  colnames(dt_score) <- paste0("SNP",1:D)
  dt_MRPC_score <- cbind(dt_score,data_ind[,1:D])

  
  #################### MRPC single SNP ###########
  dt_MRPC <- as.data.frame(dt_MRPC)
  ptm <- Sys.time()
  x.inv1 <- try(Method_MRPC(data=dt_MRPC,g=D,
                            method="disCItest"), silent=TRUE)
  if ('try-error' %in% class(x.inv1)){
    res2 <- rep(NA,12)
    time2 <- NA
  } else{
    result2 <- x.inv1
    time2 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
    Adj_directed <- as(result2@graph, "matrix")
    Adj_directed <- Adj_directed[names(data_sum),names(data_sum)]

    res2 <- unlist(calc_metrics(X=Adj_directed, X_true=R))
  }
  rm(x.inv1,Adj_directed)
  print(paste0("method 2 ",x))

  #################### MRPC score ###########
  dt_MRPC_score <- as.data.frame(dt_MRPC_score)
  ptm <- Sys.time()
  x.inv1 <- try(Method_MRPC(data=dt_MRPC_score,g=D,
                            method="gaussCItest"), silent=TRUE)
  if ('try-error' %in% class(x.inv1)){
    res21 <- rep(NA,12)
    time21 <- NA
  } else{
    result2 <- x.inv1
    time21 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
    Adj_directed <- as(result2@graph, "matrix")
    Adj_directed <- Adj_directed[names(data_sum),names(data_sum)]

    res21 <- unlist(calc_metrics(X=Adj_directed, X_true=R))
  }
  rm(x.inv1,Adj_directed,data_sum)
  print(paste0("method 21 ",x))
  
  #################### HC+prior single ####################
  dt_MRPC <- apply(dt_MRPC,2,as.numeric)
  dt_MRPC <- as.data.frame(dt_MRPC)
  ptm <- Sys.time()
  x.inv1 <- try(Method_HC(dt_MRPC,D), silent=TRUE)
  if ('try-error' %in% class(x.inv1)){
    res3 <- rep(NA,12)
    time3 <- NA
  } else{
    result3 <- x.inv1
    time3 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
    res3 <- unlist(calc_metrics(X=result3, X_true=R))
  }
  rm(x.inv1)
  print(paste0("method 3 ",x))
  
  
  #################### HC+prior score ####################
  
  ptm <- Sys.time()
  x.inv1 <- try(Method_HC(dt_MRPC_score,D), silent=TRUE)
  if ('try-error' %in% class(x.inv1)){
    res31 <- rep(NA,12)
    time31 <- NA
  } else{
    result3 <- x.inv1
    time31 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
    res31 <- unlist(calc_metrics(X=result3, X_true=R))
  }
  rm(x.inv1)
  print(paste0("method 31 ",x))
  
  
  #################### Bimmer ####################
  ptm <- Sys.time()
  x.inv1 <- try(BIMMER_Algorithm(beta,data_sum_beta,data_sum_se,D), silent=TRUE)
  if ('try-error' %in% class(x.inv1)){
    res4 <- rep(NA,12)
    time4 <- NA
  } else{
    result4 <- x.inv1
    time4 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
    res4 <- unlist(calc_metrics(X=result4$amatt, X_true=R))
  }
  rm(x.inv1)
  print(paste0("method 4 ",x))
  
  
  
  #################### cGAUGE ivw ##########################################
  ptm <- Sys.time()
  x.inv1 <- try(cGAUGE_algorithm(data_ind,D,R,beta,mmethod="ivw"), silent=F)
  if ('try-error' %in% class(x.inv1)){
    res51 <- rep(NA,12)
    time51 <- NA

  } else{
    result51 <- x.inv1
    time51 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
    res51 <- unlist(calc_metrics(X=result51$amatt, X_true=R))

  }

  #################### cGAUGE egger ##########################################
  ptm <- Sys.time()
  x.inv1 <- try(cGAUGE_algorithm(data_ind,D,R,beta,mmethod="egger"), silent=F)
  if ('try-error' %in% class(x.inv1)){

    res52 <- rep(NA,12)
    time52 <- NA

  } else{

    result52 <- x.inv1
    time52 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
    res52 <- unlist(calc_metrics(X=result52$amatt, X_true=R))

  }

  #################### cGAUGE mrpresso ##########################################
  ptm <- Sys.time()
  x.inv1 <- try(cGAUGE_algorithm(data_ind,D,R,beta,mmethod="mrpresso"), silent=F)
  if ('try-error' %in% class(x.inv1)){
    res53 <- rep(NA,12)
    time53 <- NA
  } else{
    result53 <- x.inv1
    time53 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
    res53 <- unlist(calc_metrics(X=result53$amatt_mrpresso, X_true=R))
  }

  print(paste0("method 5-3 =",x))
  
  
  print(paste0("All simul ",x," over"))
  
  ret1 <- list()
  ret5 <- list()
  ret6 <- list()
  ret7 <- list()
  for(mm in 1:length(method_name)){
    ret1[[mm]] <- res1[[method_name[mm]]]$res
    ret5[[mm]] <- res1[[method_name[mm]]]$order_met
    ret6[[mm]] <- res1[[method_name[mm]]]$res11_step1
    ret7[[mm]] <- res1[[method_name[mm]]]$amatt_step1
  }

  ret2 <- list(res2=res2,
              res3=res3,
              res21=res21,
              res31=res31,
              res4=res4,
              res51=res51,res52=res52,res53=res53,
              r2=rr,Fsta=ff)
  
  ret3 <- list()
  for(mm in 1:length(method_name)){
    ret3[[mm]] <- res1[[method_name[mm]]]$time
  }
  
  ret4 <- list(time2=time2,
               time3=time3,
               time21=time21,
               time31=time31,
               time4=time4,
               time51=time51,time52=time52,time53=time53)
  
  ret <- list(ret1,ret2,ret3,ret4,ret5,ret6,ret7)
              
              
  return(ret)
  
}
