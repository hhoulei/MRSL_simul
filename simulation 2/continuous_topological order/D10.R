
library(readr)

##########################################################################
source("Comp.R")
cutoff=5e-02

NN=100
mc=100
N=10000
D=10
gl=0.03
gub=c(0.3,0.2,0.14,0.09,0.08,0.07)[3]
dlB=0.25
duB=0.5

each_g0=c(5,seq(10,50,10))[3]
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
                   .export = c("STEP1.cpp", "STEP2.cpp", "DFS.cpp")) %dopar% {
                     #withTimeout({
                     Comp(x,N,D,each_g,gl,gu,prob,dlB,duB)
                     #},timeout = 960, onTimeout = "silent")
                   }
    
    filename0 <- paste0("D_",D,"_dlB_",dlB,"_duB_",duB,
                        "_prob_",prob,".Rdata")
    save(tt1,file=filename0)
    
  }
}

rm(list=ls())
