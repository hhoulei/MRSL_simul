

library(bnlearn)
library(MRPC)
library(pcalg)
library(doMC)
library(doParallel)
library(foreach)
library(dagitty)
library(bimmer)

library(Rcpp)
library(RcppArmadillo)

library(readr)
library(openxlsx)

######################### m2 ##################################
# source("Comp_fix.R")
# mc=50
# cutoff=5e-02
# 
# NN=100
# N=10000
# gl=0.05
# gu=0.2
# dlB=round(log(1),2)
# duB=round(log(1.5),2)
# 
# 
# each_g0=seq(10,90,20)
# 
# D_beta <- read.xlsx('/home/houlei/fixed_graph/graph_matrix.xlsx',sheet='asia',rowNames = T)
# 
# 
# for(ku in 1:length(each_g0)){
#   each_g <- each_g0[ku]
# 
#   
#   registerDoMC(mc)
#   tt1 <- foreach(x=1:NN,
#                  .packages = c("bnlearn","MRPC","pcalg","bimmer",
#                                "dagitty","Rcpp","RcppArmadillo",
#                                "MendelianRandomization","MRPRESSO"),
#                  .export = c("STEP1.cpp", "STEP2.cpp")) %dopar% {
#                    Comp(x,N,each_g,gl,gu,dlB,duB,D_beta)
#                  }
#   
#   result_once <- lapply(tt1,unlist)
#   
#   
#   if(!is.null(result_once)) {
#     
#     result_once <- unlist(result_once)
#     result_once <- matrix(result_once,nrow=NN,byrow = T)
#     
#     result_once <- cbind(each_g,result_once)
# 
#     filename0 <- paste0("asia_each_g_",each_g,"_dlB_",dlB,"_duB_",duB,".csv")
#     result_once <- as.data.frame(result_once)
#     write_csv(result_once,filename0,append = T)
#   }
#   
#   rm(each_g,result_once)
#   
# }

######################### m2 ##################################
source("Comp_fix.R")
mc=50
cutoff=5e-02

NN=100
N=10000
gl=0.05
gu=0.2
dlB=round(log(1.5),2)
duB=round(log(2),2)


each_g0=seq(10,90,20)

D_beta <- read.xlsx('/home/houlei/fixed_graph/graph_matrix.xlsx',sheet='asia',rowNames = T)


for(ku in 1:length(each_g0)){
  each_g <- each_g0[ku]

  
  registerDoMC(mc)
  tt1 <- foreach(x=1:NN,
                 .packages = c("bnlearn","MRPC","pcalg","bimmer",
                               "dagitty","Rcpp","RcppArmadillo",
                               "MendelianRandomization","MRPRESSO"),
                 .export = c("STEP1.cpp", "STEP2.cpp")) %dopar% {
                   Comp(x,N,each_g,gl,gu,dlB,duB,D_beta)
                 }
  
  result_once <- lapply(tt1,unlist)
  
  
  if(!is.null(result_once)) {
    
    result_once <- unlist(result_once)
    result_once <- matrix(result_once,nrow=NN,byrow = T)
    
    result_once <- cbind(each_g,result_once)
    
    filename0 <- paste0("asia_each_g_",each_g,"_dlB_",dlB,"_duB_",duB,".csv")
    result_once <- as.data.frame(result_once)
    write_csv(result_once,filename0,append = T)
  }
  
  rm(each_g,result_once)
  
}

######################### m2 ##################################
source("Comp_fix.R")
mc=50
cutoff=5e-02

NN=100
N=10000
gl=0.05
gu=0.2
dlB=round(log(2),2)
duB=round(log(2.5),2)


each_g0=seq(10,90,20)

D_beta <- read.xlsx('/home/houlei/fixed_graph/graph_matrix.xlsx',sheet='asia',rowNames = T)


for(ku in 1:length(each_g0)){
  each_g <- each_g0[ku]

  
  registerDoMC(mc)
  tt1 <- foreach(x=1:NN,
                 .packages = c("bnlearn","MRPC","pcalg","bimmer",
                               "dagitty","Rcpp","RcppArmadillo",
                               "MendelianRandomization","MRPRESSO"),
                 .export = c("STEP1.cpp", "STEP2.cpp")) %dopar% {
                   Comp(x,N,each_g,gl,gu,dlB,duB,D_beta)
                 }
  
  result_once <- lapply(tt1,unlist)
  
  
  if(!is.null(result_once)) {
    
    result_once <- unlist(result_once)
    result_once <- matrix(result_once,nrow=NN,byrow = T)
    
    result_once <- cbind(each_g,result_once)
    
    filename0 <- paste0("asia_each_g_",each_g,"_dlB_",dlB,"_duB_",duB,".csv")
    result_once <- as.data.frame(result_once)
    write_csv(result_once,filename0,append = T)
  }
  
  rm(each_g,result_once)
  
}

######################### m2 ##################################
source("Comp_fix.R")
mc=50
cutoff=5e-02

NN=100
N=10000
gl=0.05
gu=0.2
dlB=round(log(2.5),2)
duB=round(log(3),2)

each_g0=seq(10,90,20)

D_beta <- read.xlsx('/home/houlei/fixed_graph/graph_matrix.xlsx',sheet='asia',rowNames = T)


for(ku in 1:length(each_g0)){
  each_g <- each_g0[ku]

  
  registerDoMC(mc)
  tt1 <- foreach(x=1:NN,
                 .packages = c("bnlearn","MRPC","pcalg","bimmer",
                               "dagitty","Rcpp","RcppArmadillo",
                               "MendelianRandomization","MRPRESSO"),
                 .export = c("STEP1.cpp", "STEP2.cpp")) %dopar% {
                   Comp(x,N,each_g,gl,gu,dlB,duB,D_beta)
                 }
  
  result_once <- lapply(tt1,unlist)
  
  
  if(!is.null(result_once)) {
    
    result_once <- unlist(result_once)
    result_once <- matrix(result_once,nrow=NN,byrow = T)
    
    result_once <- cbind(each_g,result_once)
    
    filename0 <- paste0("asia_each_g_",each_g,"_dlB_",dlB,"_duB_",duB,".csv")
    result_once <- as.data.frame(result_once)
    write_csv(result_once,filename0,append = T)
  }
  
  rm(each_g,result_once)
  
}