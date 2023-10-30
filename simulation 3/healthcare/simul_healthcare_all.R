


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

source("Comp_fix.R")
mc=25
cutoff=5e-02

NN=100
N=10000
gl=0.03
gub=c(0.35,0.24,0.17,0.12,0.11,0.09)
dlB=0
duB=0.25

each_g0=c(5,seq(10,50,10))

D_beta <- read.xlsx('/home/houlei/fixed_graph/graph_matrix.xlsx',sheet='healthcare',rowNames = T)

for(ku in 1:length(each_g0)){

  each_g <- each_g0[ku]
  gu <- gub[ku]

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

    filename0 <- paste0("healthcare_each_g_",each_g,"_dlB_",dlB,"_duB_",duB,".csv")
    result_once <- as.data.frame(result_once)
    write_csv(result_once,filename0,append = T)
  }

  rm(each_g,prob,result_once)

}


######################### m2 ##################################

source("Comp_fix.R")
mc=25
cutoff=5e-02

NN=100
N=10000
gl=0.03
gub=c(0.3,0.2,0.14,0.09,0.08,0.07)
dlB=0.25
duB=0.5

each_g0=c(5,seq(10,50,10))

D_beta <- read.xlsx('/home/houlei/fixed_graph/graph_matrix.xlsx',sheet='healthcare',rowNames = T)

for(ku in 1:length(each_g0)){
  
  each_g <- each_g0[ku]
  gu <- gub[ku]
  
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
    
    filename0 <- paste0("healthcare_each_g_",each_g,"_dlB_",dlB,"_duB_",duB,".csv")
    result_once <- as.data.frame(result_once)
    write_csv(result_once,filename0,append = T)
  }
  
  rm(each_g,prob,result_once)
  
}


######################### m2 ##################################

source("Comp_fix.R")
mc=25
cutoff=5e-02

NN=100
N=10000
gl=0.03
gub=c(0.3,0.18,0.12,0.09,0.08,0.07)
dlB=0.5
duB=0.75

each_g0=c(5,seq(10,50,10))

D_beta <- read.xlsx('/home/houlei/fixed_graph/graph_matrix.xlsx',sheet='healthcare',rowNames = T)

for(ku in 1:length(each_g0)){

  each_g <- each_g0[ku]
  gu <- gub[ku]

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

    filename0 <- paste0("healthcare_each_g_",each_g,"_dlB_",dlB,"_duB_",duB,".csv")
    result_once <- as.data.frame(result_once)
    write_csv(result_once,filename0,append = T)
  }

  rm(each_g,prob,result_once)

}


######################### m2 ##################################

source("Comp_fix.R")
mc=25
cutoff=5e-02

NN=100
N=10000
gl=0.03
gub=c(0.25,0.18,0.1,0.09,0.08,0.07)
dlB=0.75
duB=1

each_g0=c(5,seq(10,50,10))

D_beta <- read.xlsx('/home/houlei/fixed_graph/graph_matrix.xlsx',sheet='healthcare',rowNames = T)

for(ku in 1:length(each_g0)){

  each_g <- each_g0[ku]
  gu <- gub[ku]

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

    filename0 <- paste0("healthcare_each_g_",each_g,"_dlB_",dlB,"_duB_",duB,".csv")
    result_once <- as.data.frame(result_once)
    write_csv(result_once,filename0,append = T)
  }

  rm(each_g,prob,result_once)

}