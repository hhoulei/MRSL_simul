
library(readr)
##########################################################################
source("Comp_binary.R")
cutoff=5e-02

NN=100
mc=50
N=10000
D=5
gl=0.05
gu=0.2
dlB=round(log(1),2)
duB=round(log(1.5),2)

each_g0=seq(10,110,20)
prob0=c(0.2,0.5,0.8)

oo=1
result_all <- NULL


for(ku in 1:length(each_g0)){
  for(kb in 1:length(prob0)){
    each_g <- each_g0[ku]
    prob <- prob0[kb]
    
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
source("Comp_binary.R")
cutoff=5e-02

NN=100
mc=50
N=10000
D=5
gl=0.05
gu=0.2
dlB=round(log(1.5),2)
duB=round(log(2),2)

each_g0=seq(10,110,20)
prob0=c(0.2,0.5,0.8)

oo=1
result_all <- NULL


for(ku in 1:length(each_g0)){
  for(kb in 1:length(prob0)){
    each_g <- each_g0[ku]
    prob <- prob0[kb]
    
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
source("Comp_binary.R")
cutoff=5e-02

NN=100
mc=50
N=10000
D=5
gl=0.05
gu=0.2
dlB=round(log(2),2)
duB=round(log(2.5),2)

each_g0=seq(10,110,20)
prob0=c(0.2,0.5,0.8)

oo=1
result_all <- NULL


for(ku in 1:length(each_g0)){
  for(kb in 1:length(prob0)){
    each_g <- each_g0[ku]
    prob <- prob0[kb]
    
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
source("Comp_binary.R")
cutoff=5e-02

NN=100
mc=50
N=10000
D=5
gl=0.05
gu=0.2
dlB=round(log(2.5),2)
duB=round(log(3),2)

each_g0=seq(10,110,20)
prob0=c(0.2,0.5,0.8)

oo=1
result_all <- NULL


for(ku in 1:length(each_g0)){
  for(kb in 1:length(prob0)){
    each_g <- each_g0[ku]
    prob <- prob0[kb]

    
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



