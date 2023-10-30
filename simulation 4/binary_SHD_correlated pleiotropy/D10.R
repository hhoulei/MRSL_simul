
library(openxlsx)
D_beta <- read.xlsx('graph_matrix.xlsx',
                    sheet='asia',rowNames = T)
D_beta_M <- read.xlsx('graph_matrix.xlsx',
                      sheet='asia_step1',rowNames = T)
D_beta_M[is.na(D_beta_M)] <- 0
############################## edge effect 0.25-0.5 ######################
############################## weak IVs #####################
source("Comp.R")
cutoff=5e-02

NN=50
mc=50
N=10000
gl=0.05
gu=0.2
dlB=round(log(1.5),2)
duB=round(log(2),2)

each_g=50
prob0=0
prob_corp0 = c(0,0.1,0.3,0.5,0.8)

oo=1
result_all <- NULL


for(ku in 1:length(prob_corp0)){
  for(kb in 1:length(prob0)){
    prob_corp <- prob_corp0[ku]

    
    registerDoMC(mc)
    tt1 <- foreach(x=1:NN,
                   .packages = c("bnlearn","MRPC","pcalg",
                                 "dagitty","Rcpp","RcppArmadillo",
                                 "MendelianRandomization","MRPRESSO",
                                 "R.utils","mr.raps","MRMix","MRCD"),
                   .export = c("STEP1.cpp", "STEP2.cpp","DFS.cpp")) %dopar% {
                     #withTimeout({
                     Comp(x,N,D_beta,each_g,gl,gu,dlB,duB,prob_corp,D_beta_M)
                     #},timeout = 960, onTimeout = "silent")
                   }
   # tt1 <- Comp(x=1,N,D_beta,each_g,gl,gu,dlB,duB,prob_corp,D_beta_M)

    # tt1 <- list()
    # for(yt in 1:NN){
    #   tt1[[yt]] <- Comp(x=yt,N,D,each_g,gl,gu,prob,dlB,duB)
    # }
    
    filename0 <- paste0("Asia_","_prob_corp_",prob_corp,"_addorder.Rdata")
    save(tt1,file=filename0)
    
    rm(tt1)
    
  }
  
}



#rm(list=ls())

