


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

source("Comp_fix.R")
#source("/home/houlei/20211222/cGAUGE-master/cGAUGE-master/R/cGAUGE.R")

main <- function(NN,N,each_g,share_g,gl,gu,dlB,duB,D_beta,mc){
  
  # windows
  # cl<- makeCluster(mc)      
  # registerDoParallel(cl)   
 
  # linux
  registerDoMC(mc)
  tt1 <- foreach(x=1:NN,
                    .packages = c("bnlearn","MRPC","pcalg","bimmer",
                                  "dagitty","Rcpp","RcppArmadillo",
                                  "MendelianRandomization","MRPRESSO"),
                    .export = c("STEP1.cpp", "STEP2.cpp")) %dopar% {
                      Comp(x,N,each_g,share_g,gl,gu,dlB,duB,D_beta)
                    }
  
  result <- lapply(tt1,unlist)
  
  
  return(result)
}





