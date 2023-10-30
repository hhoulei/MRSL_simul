library(MendelianRandomization)
library(mr.raps)
library(MRMix)
library(MRCD)

source('/home/houlei/20220402_final_fixedvariance/major revision/BiDirectCausal-main/R/BiDirCDcML.R')
source('/home/houlei/20220402_final_fixedvariance/major revision/BiDirectCausal-main/R/CDMethods.R')
source('/home/houlei/20220402_final_fixedvariance/major revision/BiDirectCausal-main/R/CD_Ratio_Independent_New.R')


source('/home/houlei/20220402_final_fixedvariance/major revision/MRcML-main/R/mr_cML_DP.R')
source('/home/houlei/20220402_final_fixedvariance/major revision/MRcML-main/R/cML_estimate_random.R')
source('/home/houlei/20220402_final_fixedvariance/major revision/MRcML-main/R/cML_estimate.R')
source('/home/houlei/20220402_final_fixedvariance/major revision/MRcML-main/R/cML_estimate_DP.R')
source('/home/houlei/20220402_final_fixedvariance/major revision/MRcML-main/R/cML_SdTheta.R')

# bx=runif(50,0.05,0.2)
# bxse=runif(50,0.05,0.1)
# by=bx*0.2+rnorm(50,0,1)
# byse=runif(50,0.01,0.15)
# N=1000


Method_UVMR <- function(bx, bxse, by, byse,N,method_name){
  
  result <- NULL
  
  mr_frame<-as.data.frame(cbind(bx,bxse,by,byse))
  names(mr_frame)<- c('bx','bxse','by','byse')
  mr_object<- mr_input(bx, bxse, by, byse) 
  
  if(method_name=='weighted median'){
    #perform weighted median
    cat('perform weighted median ... \n')
    res1 <- MendelianRandomization::mr_median(mr_object)
    result <- rbind(result,
                    c(res1@Estimate,res1@StdError,res1@CILower,res1@CIUpper,res1@Pvalue))
  }else if(method_name=='Mode based'){
    #perform Mode based estimation
    cat('perform Mode based ... \n')
    res2 <- MendelianRandomization::mr_mbe(mr_object)
    result <- rbind(result,
                    c(res2@Estimate,res2@StdError,res2@CILower,res2@CIUpper,res2@Pvalue))
  }else if(method_name=='MR.Mix'){
    # perform MR-Mix
    cat('perform MR Mix ... \n')
    res5 = MRMix(bx, by, bxse, byse, profile = TRUE)
    result <- rbind(result,
                    c(res5$theta,res5$SE_theta,NA,NA,res5$pvalue_theta))
  }else if(method_name=='MR.RAPS'){
    # perform MR-RAPS with Huber loss function
    cat('perform MR RAPS ... \n')
    res6 <- mr.raps(bx, by, bxse, byse)
    result <- rbind(result,
                    c(res6$beta.hat,res6$beta.se,NA,NA,res6$beta.p.value))
  }else if(method_name=='mrcML.DP'){
    # mrcML_DP
    cat('perform mrcML_DP ... \n')
    res7 = mr_cML_DP(bx,
                     by,
                     bxse,
                     byse,
                     n = N)
    result <- rbind(result,
                    c(res7$MA_BIC_DP_theta,res7$MA_BIC_DP_se,NA,NA,res7$MA_BIC_DP_p))
  }else if(method_name=='CDcML.DP.S'){
    #CDcML_DP
    cat('perform CDcML_DP ... \n')
    res8 <- BiDirCDcML(b_X = bx,
                       b_Y = by,
                       se_X = bxse,
                       se_Y = byse,
                       n_X = N,
                       n_Y = N,
                       sig.cutoff = 0.05/20)
    result <- rbind(result,
                    c(res8$XtoY.est.S.DP,
                      res8$XtoY.se.S.DP,
                      res8$XtoY.est.S.DP-1.96*res8$XtoY.se.S.DP,
                      res8$XtoY.est.S.DP+1.96*res8$XtoY.se.S.DP,
                      NA))
  }else if(method_name %in% c('CD.Ratio','CD.Egger')){
    #CDMethods
    cat('perform CDMethods ... \n')
    res9 <- CDMethods(bx,by,bxse,byse,n_exp=N,n_out=N)
    res91 <- res9$CD_Ratio_result$T1toT2
    res92 <- res9$CD_Egger_result$T1toT2
    
    if(method_name=='CD.Ratio'){
      result <- rbind(result,
                      c(res91[1],
                        res91[2],
                        res91[1]-1.96*res91[2],
                        res91[1]+1.96*res91[2],
                        NA))
    }else if(method_name=='CD.Egger'){
      result <- rbind(result,
                      c(res92[2],
                        res92[4],
                        res92[2]-1.96*res92[4],
                        res92[2]+1.96*res92[4],NA))
    }
  }
  
  # #perform MR-Robust
  # res3 <- MendelianRandomization::mr_ivw(mr_object,"random", robust = TRUE)
  # result <- rbind(result,
  #                 c(res3@Estimate,res3@StdError,res3@CILower,res3@CIUpper,res3@Pvalue))
  

  #perform contamination mixture, note we removed the 27th variable due to having a ratio to close to infty.
  # res4 <- MendelianRandomization::mr_conmix(mr_object)
  # result <- rbind(result,
  #                 c(res4@Estimate,NA,res4@CILower,res4@CIUpper,res4@Pvalue))

  result <- as.data.frame(result)
  colnames(result) <- c('beta','se','low','up','pval')
  result$method <- method_name
  
  if(is.na(result$pval)){
    
    if(is.na(result$low)){
      Direct <- NA
    }else{
      if(result$low<0 & result$up>0){
        Direct <- 0
      }else{
        Direct <- 1
      }
    }
  }else{
    Direct <- ifelse(result$pval<0.05,1,0)
  }
  
  result$direction <- Direct
  
  return(result)
  
}


STEP1_pleio <- function(data_sum_beta,data_sum_se,N,method_name,beta){
  
  D <- ncol(data_sum_beta)
  amatt_pleio <- matrix(0,nrow=D,ncol=D)
  
  for(i in 1:D){
    for(j in 1:D){
      
      cat('STEP1_pleio: i=',i,' j=',j,'\n')
      
      if(i==j) next
      
      llp <- which(beta[,i]!=0)
      
      res <- Method_UVMR(bx=data_sum_beta[llp,i], 
                         bxse=data_sum_se[llp,i], 
                         by=data_sum_beta[llp,j], 
                         byse=data_sum_se[llp,j],
                         N,method_name)
      amatt_pleio[i,j] <- res$direction
    }
  }
  
  return(amatt_pleio)
}

