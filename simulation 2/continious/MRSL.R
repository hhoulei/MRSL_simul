
Rcpp::sourceCpp('STEP1.cpp')
Rcpp::sourceCpp('STEP2.cpp')

MRSL <- function(data_sum_beta,data_sum_se,beta,cutoff,
                 adj_methods,use_eggers_step1,use_eggers_step2,vary_mvmr_adj){
  
  
  res1 <- STEP1(data_sum_beta,data_sum_se,beta,use_eggers_step1)
  
  amatt0 <- list()
  amatt0[[1]] <- res1$amatt
  just <- TRUE
  i=1
  while(just){
    cat("iteration--",i)
    i=i+1
    amatt055 <- STEP2(amatt0[[i-1]],data_sum_beta,data_sum_se,beta,
                  adj_methods,use_eggers_step2,vary_mvmr_adj)
    amatt0[[i]] <- amatt055$amatt
    just <- !all(amatt0[[i]]==amatt0[[i-1]])
  }
  
  return(list(amatt=amatt0[[i]],
              iteration=i))
}