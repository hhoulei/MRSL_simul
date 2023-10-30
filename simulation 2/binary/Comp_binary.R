
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

source("DataGeneration_binary.R")
source("Algorithm.R")
source("MRSL.R")


Comp <- function(x,N,D,each_g,gl,gu,prob,dlB,duB){
  
  fdata <- DataGeneration_binary(N,D,each_g,gl,gu,prob,dlB,duB)
  R <- fdata$D_beta
  data_ind <- fdata$data_ind
  data_sum <- fdata$data_sum
  beta <- fdata$G_beta
  
  # rr <- NULL
  # ff <- NULL
  # for(hh in 1:D){
  #   total_g <- which(beta[,hh]!=0)+D
  #   dd_G <- as.data.frame(data_ind[,total_g])
  #   fit <- lm(data_ind[,hh]~.,dd_G)
  #   ff <- c(ff,summary(fit)$fstatistic[1])
  #   rr <- c(rr,summary(fit)$r.squared)
  # }
  
  data_sum_beta <- NULL
  data_sum_se <- NULL
  #data_sum_pval <- NULL
  for(oi in 1:length(data_sum)){
    data_sum_beta <- cbind(data_sum_beta,data_sum[[oi]][,1])
    data_sum_se <- cbind(data_sum_se,data_sum[[oi]][,2])
    #data_sum_pval <- cbind(data_sum_pval,data_sum[[oi]][,3])
  }


  #################### MRSL ##########################################
  
  ############## not vary mvmr adjustment nodes ###########
  
  #remove collier
  
  ptm <- Sys.time()
  x.inv1 <- try(MRSL(data_sum_beta,data_sum_se,beta,
                     adj_methods=1,use_eggers_step1=0,use_eggers_step2=0,
                     vary_mvmr_adj=0), silent=TRUE)
  # adj_methods: 1 remove collier; 2 variables in open path; 3 min sufficient sets
  
  if ('try-error' %in% class(x.inv1)){
    res11 <- rep(NA,11)
    time11 <- NA
  } else{
    result1 <- x.inv1
    time11 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
    res11 <- unlist(calc_metrics(X=result1$amatt, X_true=R))
  }
  rm(x.inv1)
  
  print(paste0("method 1 - 1 ",x))
  
  #open path
  
  ptm <- Sys.time()
  x.inv1 <- try(MRSL(data_sum_beta,data_sum_se,beta,
                     adj_methods=2,use_eggers_step1=0,use_eggers_step2=0,
                     vary_mvmr_adj=0), silent=TRUE)
  if ('try-error' %in% class(x.inv1)){
    res12 <- rep(NA,11)
    time12 <- NA
  } else{
    result1 <- x.inv1
    time12 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
    res12 <- unlist(calc_metrics(X=result1$amatt, X_true=R))
  }
  rm(x.inv1)
  
  print(paste0("method 1 - 2 ",x))
  
  #min sufficient
  
  ptm <- Sys.time()
  x.inv1 <- try(MRSL(data_sum_beta,data_sum_se,beta,
                     adj_methods=3,use_eggers_step1=0,use_eggers_step2=0,
                     vary_mvmr_adj=0), silent=TRUE)
  if ('try-error' %in% class(x.inv1)){
    res13 <- rep(NA,11)
    time13 <- NA
  } else{
    result1 <- x.inv1
    time13 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
    res13 <- unlist(calc_metrics(X=result1$amatt, X_true=R))
  }
  rm(x.inv1)
  
  print(paste0("method 1 - 3 ",x))
  
  
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
    res2 <- rep(NA,11)
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
    res21 <- rep(NA,11)
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
    res3 <- rep(NA,11)
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
    res31 <- rep(NA,11)
    time31 <- NA
  } else{
    result3 <- x.inv1
    time31 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
    res31 <- unlist(calc_metrics(X=result3, X_true=R))
  }
  rm(x.inv1)
  print(paste0("method 31 ",x))
  
  
  # #################### Bimmer ####################
  # ptm <- Sys.time()
  # x.inv1 <- try(BIMMER_Algorithm(beta,data_sum_beta,data_sum_se,D), silent=TRUE)
  # if ('try-error' %in% class(x.inv1)){
  #   res4 <- rep(NA,11)
  #   time4 <- NA
  # } else{
  #   result4 <- x.inv1
  #   time4 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
  #   res4 <- unlist(calc_metrics(X=result4$amatt, X_true=R))
  # }
  # rm(x.inv1)
  # print(paste0("method 4 ",x))
  
  
  
  #################### cGAUGE ivw ##########################################
  ptm <- Sys.time()
  x.inv1 <- try(cGAUGE_algorithm(data_ind,D,R,beta,mmethod="ivw"), silent=F)
  if ('try-error' %in% class(x.inv1)){
    res51 <- rep(NA,11)
    time51 <- NA

  } else{
    result51 <- x.inv1
    time51 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
    res51 <- unlist(calc_metrics(X=result51$amatt, X_true=R))

  }

  #################### cGAUGE ivw ##########################################
  ptm <- Sys.time()
  x.inv1 <- try(cGAUGE_algorithm(data_ind,D,R,beta,mmethod="egger"), silent=F)
  if ('try-error' %in% class(x.inv1)){

    res52 <- rep(NA,11)
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
    res53 <- rep(NA,11)
    time53 <- NA
  } else{
    result53 <- x.inv1
    time53 <- as.numeric(difftime(Sys.time(),ptm,units = "secs"))
    res53 <- unlist(calc_metrics(X=result53$amatt_mrpresso, X_true=R))
  }

  print(paste0("method 5-3 =",x))
  
  
  print(paste0("All simul ",x," over"))
  
  ret <- list(res11=res11,res12=res12,res13=res13,
              res2=res2,res3=res3,res21=res21,res31=res31,
             res51=res51,res52=res52,res53=res53,
              #r2=rr,Fsta=ff,
              time11=time11, time12=time12, time13=time13,
              time2=time2,time3=time3,time21=time21,time31=time31,
              time51=time51,time52=time52,time53=time53)
  return(ret)
  
}
