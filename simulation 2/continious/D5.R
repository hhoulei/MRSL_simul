
library(readr)
##########################################################################
source("Comp.R")
cutoff=5e-02

NN=100
mc=50
N=10000
D=5
gl=0.04
gub=c(0.35,0.24,0.17,0.12,0.11,0.09)
dlB=0
duB=0.25


each_g0=c(5,seq(10,50,10))
prob0=c(0.2,0.5,0.8)

oo=1
result_all <- NULL


for(ku in 1:length(each_g0)){
  for(kb in 1:length(prob0)){
    each_g <- each_g0[ku]
    prob <- prob0[kb]
    gu <- gub[ku]

    registerDoMC(mc)
    tt1 <- foreach(x=1:NN,
                   .packages = c("bnlearn","MRPC","pcalg",
                                 "dagitty","Rcpp","RcppArmadillo",
                                 "MendelianRandomization","MRPRESSO",
                                 "R.utils"),
                   .export = c("STEP1.cpp", "STEP2.cpp")) %dopar% {
                     #withTimeout({
                     Comp(x,N,D,each_g,gl,gu,prob,dlB,duB)
                     #},timeout = 960, onTimeout = "silent")
                   }

    result_once <- lapply(tt1,unlist)
    result_once <- unlist(result_once)

    if(!is.null(result_once)) {

      result_once <- matrix(result_once,nrow=NN,byrow = T)

      result_once <- cbind(prob,result_once)
      result_once <- cbind(each_g,result_once)

      filename0 <- paste0("D_",D,"_dlB_",dlB,"_duB_",duB,"_",oo,".csv")
      oo=oo+1
      result_once <- as.data.frame(result_once)
      write_csv(result_once,filename0,append = T)
    }

    rm(each_g,prob,result_once)

  }
}

rm(list=ls())


##########################################################################
source("Comp.R")
cutoff=5e-02

NN=100
mc=50
N=10000
D=5
gl=0.03
gub=c(0.3,0.2,0.14,0.09,0.08,0.07)
dlB=0.25
duB=0.5

each_g0=c(5,seq(10,50,10))
prob0=c(0.2,0.5,0.8)

oo=1
result_all <- NULL


for(ku in 1:length(each_g0)){
  for(kb in 1:length(prob0)){
    each_g <- each_g0[ku]
    prob <- prob0[kb]
    gu <- gub[ku]
    
    registerDoMC(mc)
    tt1 <- foreach(x=1:NN,
                   .packages = c("bnlearn","MRPC","pcalg",
                                 "dagitty","Rcpp","RcppArmadillo",
                                 "MendelianRandomization","MRPRESSO",
                                 "R.utils"),
                   .export = c("STEP1.cpp", "STEP2.cpp")) %dopar% {
                     #withTimeout({
                     Comp(x,N,D,each_g,gl,gu,prob,dlB,duB)
                     #},timeout = 960, onTimeout = "silent")
                   }
    
    result_once <- lapply(tt1,unlist)
    result_once <- unlist(result_once)
    
    if(!is.null(result_once)) {
      
      result_once <- matrix(result_once,nrow=NN,byrow = T)
      
      result_once <- cbind(prob,result_once)
      result_once <- cbind(each_g,result_once)
      
      filename0 <- paste0("D_",D,"_dlB_",dlB,"_duB_",duB,"_",oo,".csv")
      oo=oo+1
      result_once <- as.data.frame(result_once)
      write_csv(result_once,filename0,append = T)
    }
    
    rm(each_g,prob,result_once)
    
  }
}

rm(list=ls())

##########################################################################
source("Comp.R")
cutoff=5e-02

NN=100
mc=50
N=10000
D=5
gl=0.03
gub=c(0.3,0.18,0.12,0.09,0.08,0.07)
dlB=0.5
duB=0.75

each_g0=c(5,seq(10,50,10))
prob0=c(0.2,0.5,0.8)

oo=1
result_all <- NULL


for(ku in 1:length(each_g0)){
  for(kb in 1:length(prob0)){
    each_g <- each_g0[ku]
    prob <- prob0[kb]
    gu <- gub[ku]
    
    registerDoMC(mc)
    tt1 <- foreach(x=1:NN,
                   .packages = c("bnlearn","MRPC","pcalg",
                                 "dagitty","Rcpp","RcppArmadillo",
                                 "MendelianRandomization","MRPRESSO",
                                 "R.utils"),
                   .export = c("STEP1.cpp", "STEP2.cpp")) %dopar% {
                     #withTimeout({
                     Comp(x,N,D,each_g,gl,gu,prob,dlB,duB)
                     #},timeout = 960, onTimeout = "silent")
                   }
    
    result_once <- lapply(tt1,unlist)
    result_once <- unlist(result_once)
    
    if(!is.null(result_once)) {
      
      result_once <- matrix(result_once,nrow=NN,byrow = T)
      
      result_once <- cbind(prob,result_once)
      result_once <- cbind(each_g,result_once)
      
      filename0 <- paste0("D_",D,"_dlB_",dlB,"_duB_",duB,"_",oo,".csv")
      oo=oo+1
      result_once <- as.data.frame(result_once)
      write_csv(result_once,filename0,append = T)
    }
    
    rm(each_g,prob,result_once)
    
  }
}

rm(list=ls())


##########################################################################
source("Comp.R")
cutoff=5e-02

NN=100
mc=50
N=10000
D=5
gl=0.03
gub=c(0.25,0.18,0.1,0.09,0.08,0.07)
dlB=0.75
duB=1

each_g0=c(5,seq(10,50,10))
prob0=c(0.2,0.5,0.8)

oo=1
result_all <- NULL


for(ku in 1:length(each_g0)){
  for(kb in 1:length(prob0)){
    each_g <- each_g0[ku]
    prob <- prob0[kb]
    gu <- gub[ku]
    
    registerDoMC(mc)
    tt1 <- foreach(x=1:NN,
                   .packages = c("bnlearn","MRPC","pcalg",
                                 "dagitty","Rcpp","RcppArmadillo",
                                 "MendelianRandomization","MRPRESSO",
                                 "R.utils"),
                   .export = c("STEP1.cpp", "STEP2.cpp")) %dopar% {
                     #withTimeout({
                     Comp(x,N,D,each_g,gl,gu,prob,dlB,duB)
                     #},timeout = 960, onTimeout = "silent")
                   }
    
    result_once <- lapply(tt1,unlist)
    result_once <- unlist(result_once)
    
    if(!is.null(result_once)) {
      
      result_once <- matrix(result_once,nrow=NN,byrow = T)
      
      result_once <- cbind(prob,result_once)
      result_once <- cbind(each_g,result_once)
      
      filename0 <- paste0("D_",D,"_dlB_",dlB,"_duB_",duB,"_",oo,".csv")
      oo=oo+1
      result_once <- as.data.frame(result_once)
      write_csv(result_once,filename0,append = T)
    }
    
    rm(each_g,prob,result_once)
    
  }
}

rm(list=ls())



