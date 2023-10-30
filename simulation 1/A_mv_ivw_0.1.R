
# A graph 


library(foreach)
library(doParallel)
library(doMC)
Datageneration <- function(N,g,elow,eup,method,b){
  
  if(method==1){
    
    gx <- g
    GX <- NULL
    for(i in 1:gx){
      Gg <- rbinom(N,2,0.3)
      GX <- cbind(GX,Gg)
    }
    U <- rnorm(N,0,1)
    X <- GX %*% runif(gx,0.05,0.2) + U + rnorm(N,0,1)
    Y <- b*X + U + rnorm(N,0,1)
    M2 <- runif(1,elow,eup)*X + runif(1,elow,eup)*Y + 
      GX %*% rnorm(gx,0,0.2^2) + U + rnorm(N,0,1)

    G <- GX
    
  }else if(method==2){
    
    G <- NULL
    for(i in 1:g){
      Gg <- rbinom(N,2,0.3)
      G <- cbind(G,Gg)
    }
    U <- rnorm(N,0,1)
    X <- G %*% runif(g,0.05,0.2) + U + rnorm(N,0,1)
    Y <-  b*X + U + rnorm(N,0,1)
    M2 <- G %*% runif(g,0.05,0.2) + 
      runif(1,elow,eup)*X + runif(1,elow,eup)*Y + U + rnorm(N,0,1)
    
  }else if(method==3){
    
    G <- NULL
    for(i in 1:g){
      Gg <- rbinom(N,2,0.3)
      G <- cbind(G,Gg)
    }
    U <- rnorm(N,0,1)
    X <- U + rnorm(N,0,1)
    Y <-  b*X + U + rnorm(N,0,1)
    M2 <- G %*% runif(g,0.05,0.2) + 
      runif(1,elow,eup)*X + runif(1,elow,eup)*Y + U + rnorm(N,0,1)
    
  }else if(method==4){
    
    gx <- g2 <- g/2
    GX <- NULL
    for(i in 1:gx){
      Gg <- rbinom(N,2,0.3)
      GX <- cbind(GX,Gg)
    }
    G2 <- NULL
    for(i in 1:g2){
      Gg <- rbinom(N,2,0.3)
      G2 <- cbind(G2,Gg)
    }
    U <- rnorm(N,0,1)
    X <- GX %*% runif(gx,0.05,0.2) + U + rnorm(N,0,1)
    Y <-  b*X + U + rnorm(N,0,1)
    M2 <- G2 %*% runif(g2,0.05,0.2) + 
      runif(1,elow,eup)*X + runif(1,elow,eup)*Y + U + rnorm(N,0,1)
    G <- cbind(GX,G2)
    
  }else if(method==5){
    
    gx <- g2 <- g/2
    GX <- NULL
    for(i in 1:gx){
      Gg <- rbinom(N,2,0.3)
      GX <- cbind(GX,Gg)
    }
    G2 <- NULL
    for(i in 1:g2){
      Gg <- rbinom(N,2,0.3)
      G2 <- cbind(G2,Gg)
    }
    U <- rnorm(N,0,1)
    X <- GX %*% runif(gx,0.05,0.2) + G2 %*% runif(g2,0.05,0.2) + U + rnorm(N,0,1)
    Y <-  b*X + U + rnorm(N,0,1)
    M2 <- G2 %*% runif(g2,0.05,0.2) + 
      runif(1,elow,eup)*X + runif(1,elow,eup)*Y + U + rnorm(N,0,1)
    G <- cbind(GX,G2)
    
  }else if(method==6){
    
    gx <- g2 <- g/2
    GX <- NULL
    for(i in 1:gx){
      Gg <- rbinom(N,2,0.3)
      GX <- cbind(GX,Gg)
    }
    G2 <- NULL
    for(i in 1:g2){
      Gg <- rbinom(N,2,0.3)
      G2 <- cbind(G2,Gg)
    }
    U <- rnorm(N,0,1)
    X <- G2 %*% runif(g2,0.05,0.2) + U + rnorm(N,0,1)
    Y <-  b*X + U + rnorm(N,0,1)
    M2 <- GX %*% runif(gx,0.05,0.2) + G2 %*% runif(g2,0.05,0.2) + 
      runif(1,elow,eup)*X + runif(1,elow,eup)*Y + U + rnorm(N,0,1)
    G <- cbind(GX,G2)
    
  }else if(method==7){
    
    gx <- g2 <- floor(g/3)
    g3 <- g-gx-g2
    GX <- NULL
    for(i in 1:gx){
      Gg <- rbinom(N,2,0.3)
      GX <- cbind(GX,Gg)
    }
    G2 <- NULL
    for(i in 1:g2){
      Gg <- rbinom(N,2,0.3)
      G2 <- cbind(G2,Gg)
    }
    G3 <- NULL
    for(i in 1:g3){
      Gg <- rbinom(N,2,0.3)
      G3 <- cbind(G3,Gg)
    }
    U <- rnorm(N,0,1)
    X <- G2 %*% runif(g2,0.05,0.2) + G3 %*% runif(g3,0.05,0.2) + U + rnorm(N,0,1)
    Y <-  b*X + U + rnorm(N,0,1)
    M2 <- GX %*% runif(gx,0.05,0.2) + G2 %*% runif(g2,0.05,0.2) + 
      runif(1,elow,eup)*X + runif(1,elow,eup)*Y + U + rnorm(N,0,1)
    G <- cbind(GX,G2,G3)
    
  }

  data <- cbind(G,X,M2,Y)
  data <- as.data.frame(data)
  colnames(data) <- c(paste0("G",1:g),'X','M2','Y')
  return(data)
}

Comp <- function(x,N,g,elow,eup,method,b){
  
  fdata <- Datageneration(N,g,elow,eup,method,b)
  
  betaXG <- NULL
  betaCG <- NULL
  betaM1G <- NULL
  betaM2G <- NULL
  betaYG <- NULL
  sebetaXG <- NULL
  sebetaCG <- NULL
  sebetaM1G <- NULL
  sebetaM2G <- NULL
  sebetaYG <- NULL
  
  for (t in 1:g) {
    
    fit1 <- lm(fdata$X~fdata[,t])
    betaXG <- c(betaXG,summary(fit1)$coef[2,1])
    sebetaXG <- c(sebetaXG,summary(fit1)$coef[2,2])
    
    fit4 <- lm(fdata$M2~fdata[,t])
    betaM2G <- c(betaM2G,summary(fit4)$coef[2,1])
    sebetaM2G <- c(sebetaM2G,summary(fit4)$coef[2,2])
    
    fit5 <- lm(fdata$Y~fdata[,t])
    betaYG <- c(betaYG,summary(fit5)$coef[2,1])
    sebetaYG <- c(sebetaYG,summary(fit5)$coef[2,2])
  }
  
  fit6 <- lm(betaYG~betaXG+betaM2G-1,weights = sebetaYG^(-2))
  result_once <- c(summary(fit6)$coef[1,])
  
  return(result_once)
}



main <- function(NN,N,g,elow,eup,method,b,mc){
  
  registerDoMC(mc)
  rerex_MVivw <- foreach(i=1:NN,.combine="rbind") %dopar% {
    Comp(x=i,N,g,elow,eup,method,b)
  }

  return(rerex_MVivw)
}

mc=25
gb <- c(6,10,20,40,60,80,100)
elowb=seq(0.1,0.9,0.2)
eupb=elowb

result <- NULL


for(tt in 1:length(elowb)){
  
  elow <- elowb[tt]
  eup <- eupb[tt]
  
  ######### 1 ##################
  result_once1 <- NULL
  for(kh in 1:length(gb)){
    result_once1 <- rbind(result_once1,
                          main(NN=1000,N=10000,gb[kh],elow,eup,method=1,b=0.1,mc))
  }
  result_once1 <- cbind(rep(gb,each=1000),result_once1)
  colnames(result_once1) <- c('g','beta','se','tvalue','pvalue')
  write.csv(result_once1,paste0('A_elow_',elow,'_eup_',eup,'method_',1,'_0.1.csv'))
  
  rere1 <- NULL
  for(uh in 1:length(gb)){
    result_once1_1 <- result_once1[result_once1[,1]==gb[uh],]
    result_once1_1 <- c(mean(result_once1_1[,2],na.rm=T),
                       sd(result_once1_1[,2],na.rm=T),
                       mean(result_once1_1[,5]<0.05,na.rm=T))
    rere1 <- rbind(rere1,result_once1_1)
  }

  ######### 2 ##################
  result_once2 <- NULL
  for(kh in 1:length(gb)){
    result_once2 <- rbind(result_once2,
                          main(NN=1000,N=10000,gb[kh],elow,eup,method=2,b=0.1,mc))
  }
  result_once2 <- cbind(rep(gb,each=1000),result_once2)
  colnames(result_once2) <- c('g','beta','se','tvalue','pvalue')
  write.csv(result_once2,paste0('A_elow_',elow,'_eup_',eup,'method_',2,'_0.1.csv'))
  
  rere2 <- NULL
  for(uh in 1:length(gb)){
    result_once2_1 <- result_once2[result_once2[,1]==gb[uh],]
    result_once2_1 <- c(mean(result_once2_1[,2],na.rm=T),
                        sd(result_once2_1[,2],na.rm=T),
                        mean(result_once2_1[,5]<0.05,na.rm=T))
    rere2 <- rbind(rere2,result_once2_1)
  }
  
  ######### 3 ##################
  result_once3 <- NULL
  for(kh in 1:length(gb)){
    result_once3 <- rbind(result_once3,
                          main(NN=1000,N=10000,gb[kh],elow,eup,method=3,b=0.1,mc))
  }
  result_once3 <- cbind(rep(gb,each=1000),result_once3)
  colnames(result_once3) <- c('g','beta','se','tvalue','pvalue')
  write.csv(result_once3,paste0('A_elow_',elow,'_eup_',eup,'method_',3,'_0.1.csv'))
  
  rere3 <- NULL
  for(uh in 1:length(gb)){
    result_once3_1 <- result_once3[result_once3[,1]==gb[uh],]
    result_once3_1 <- c(mean(result_once3_1[,2],na.rm=T),
                        sd(result_once3_1[,2],na.rm=T),
                        mean(result_once3_1[,5]<0.05,na.rm=T))
    rere3 <- rbind(rere3,result_once3_1)
  }
  
  ######### 4 ##################
  result_once4 <- NULL
  for(kh in 1:length(gb)){
    result_once4 <- rbind(result_once4,
                          main(NN=1000,N=10000,gb[kh],elow,eup,method=4,b=0.1,mc))
  }
  result_once4 <- cbind(rep(gb,each=1000),result_once4)
  colnames(result_once4) <- c('g','beta','se','tvalue','pvalue')
  write.csv(result_once4,paste0('A_elow_',elow,'_eup_',eup,'method_',4,'_0.1.csv'))
  
  rere4 <- NULL
  for(uh in 1:length(gb)){
    result_once4_1 <- result_once4[result_once4[,1]==gb[uh],]
    result_once4_1 <- c(mean(result_once4_1[,2],na.rm=T),
                        sd(result_once4_1[,2],na.rm=T),
                        mean(result_once4_1[,5]<0.05,na.rm=T))
    rere4 <- rbind(rere4,result_once4_1)
  }
  
  ######### 5 ##################
  result_once5 <- NULL
  for(kh in 1:length(gb)){
    result_once5 <- rbind(result_once5,
                          main(NN=1000,N=10000,gb[kh],elow,eup,method=5,b=0.1,mc))
  }
  result_once5 <- cbind(rep(gb,each=1000),result_once5)
  colnames(result_once5) <- c('g','beta','se','tvalue','pvalue')
  write.csv(result_once5,paste0('A_elow_',elow,'_eup_',eup,'method_',5,'_0.1.csv'))
  
  rere5 <- NULL
  for(uh in 1:length(gb)){
    result_once5_1 <- result_once5[result_once5[,1]==gb[uh],]
    result_once5_1 <- c(mean(result_once5_1[,2],na.rm=T),
                        sd(result_once5_1[,2],na.rm=T),
                        mean(result_once5_1[,5]<0.05,na.rm=T))
    rere5 <- rbind(rere5,result_once5_1)
  }
  
  ######### 6 ##################
  result_once6 <- NULL
  for(kh in 1:length(gb)){
    result_once6 <- rbind(result_once6,
                          main(NN=1000,N=10000,gb[kh],elow,eup,method=6,b=0.1,mc))
  }
  result_once6 <- cbind(rep(gb,each=1000),result_once6)
  colnames(result_once6) <- c('g','beta','se','tvalue','pvalue')
  write.csv(result_once6,paste0('A_elow_',elow,'_eup_',eup,'method_',6,'_0.1.csv'))
  
  rere6 <- NULL
  for(uh in 1:length(gb)){
    result_once6_1 <- result_once6[result_once6[,1]==gb[uh],]
    result_once6_1 <- c(mean(result_once6_1[,2],na.rm=T),
                        sd(result_once6_1[,2],na.rm=T),
                        mean(result_once6_1[,5]<0.05,na.rm=T))
    rere6 <- rbind(rere6,result_once6_1)
  }
  
  
  ######### 7 ##################
  result_once7 <- NULL
  for(kh in 1:length(gb)){
    result_once7 <- rbind(result_once7,
                          main(NN=1000,N=10000,gb[kh],elow,eup,method=7,b=0.1,mc))
  }
  result_once7 <- cbind(rep(gb,each=1000),result_once7)
  colnames(result_once7) <- c('g','beta','se','tvalue','pvalue')
  write.csv(result_once7,paste0('A_elow_',elow,'_eup_',eup,'method_',7,'_0.1.csv'))
  
  rere7 <- NULL
  for(uh in 1:length(gb)){
    result_once7_1 <- result_once7[result_once7[,1]==gb[uh],]
    result_once7_1 <- c(mean(result_once7_1[,2],na.rm=T),
                        sd(result_once7_1[,2],na.rm=T),
                        mean(result_once7_1[,5]<0.05,na.rm=T))
    rere7 <- rbind(rere7,result_once7_1)
  }
  
  
  result_once <- cbind(rere1,rere2,rere3,rere4,rere5,rere6,rere7)
  result_once <- cbind(gb,result_once)
  result_once <- cbind(eup,result_once)
  result_once <- cbind(elow,result_once)
  colnames(result_once) <- c('elow','eup','g',
                             'beta1_1','sebeta1_1','typeI1_1',
                             'beta1_2','sebeta1_2','typeI1_2',
                             'beta1_3','sebeta1_3','typeI1_3',
                             'beta1_4','sebeta1_4','typeI1_4',
                             'beta1_5','sebeta1_5','typeI1_5',
                             'beta1_6','sebeta1_6','typeI1_6',
                             'beta1_7','sebeta1_7','typeI1_7')
  
  result <- rbind(result,result_once)
}


write.csv(result,'A_mv_ivw_0.1.csv')

