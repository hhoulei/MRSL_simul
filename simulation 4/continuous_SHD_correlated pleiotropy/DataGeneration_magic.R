
# asia

DataGeneration_magic <- function(N,each_g,gl,gu,dlB,duB,D_beta,prob_corp){
  
  D <- 7
  total_g <- D*each_g + each_g
  
  # G <- NULL
  G_pri <- NULL
  for(j in 1:total_g){
    opg <- rbinom(N,2,0.3)
    G_pri <- cbind(G_pri,opg)
    # G <- cbind(G,scale(opg, center=T,scale=T))
  }
  
  # G-Y coefficients
  G_beta <- matrix(0,ncol=D,nrow=total_g)
  for(d in 1:D){
    ww <- (1 + each_g*(d-1)):(each_g*d)
    G_beta[ww,d] <- runif(length(ww),gl,gu)
  }
  G_beta[(D*each_g+1):total_g,] <- runif(each_g*D,gl,gu)
  colnames(G_beta) <- paste0("D",1:D)
  
  # D-Y coefficients
  D_beta[is.na(D_beta)] <- 0
  non_0 <- D_beta==1
  D_beta[non_0] <- runif(sum(non_0), min = dlB, max = duB)
  
  # U <- NULL
  # for(ed in 1:D){
  #   U <- cbind(U,rnorm(N,0,1))
  # }
  # data_ind <- (G %*% G_beta + U) %*% solve(diag(D)-D_beta) 
  
  #corp_eff <- c(rep(0,floor(total_g*(1-prob_corp))),
   #             rep(0.1,ceiling(total_g*prob_corp)))

  corp_eff <- c(rep(0,ceiling(total_g*(1-prob_corp))),
                        runif(floor(total_g*prob_corp),-0.1,0.1))
  U <- G_pri %*% corp_eff + rnorm(N,0,1)
  
  data_ind <- matrix(NA,nrow=N,ncol=D)
  lllo <- apply(D_beta,2,function(x) all(x==0))
  
  if(sum(lllo)!=0){
    oriD <- which(lllo)
    for(oe in 1:length(oriD)){
      # pp <- G %*% G_beta[,oriD[oe]] + rnorm(N,0,1)
      # pp1 <- exp(pp)/(1+exp(pp))
      # data_ind[,oriD[oe]] <- rbinom(N,1,pp1)
      data_ind[,oriD[oe]] <- G_pri %*% G_beta[,oriD[oe]] + rnorm(N,0,1)
    }
  }
  
  if(sum(!lllo)!=0){
    aftD <- which(!lllo)
    for(oe in 1:length(aftD)){
      parennode <- which(D_beta[,aftD[oe]]!=0)
      if(length(parennode)==1){
        pp <- data_ind[,parennode] * D_beta[parennode,aftD[oe]] + G_pri %*% G_beta[,aftD[oe]] + U + rnorm(N,0,1)
      }else{
        pp <- data_ind[,parennode] %*% D_beta[parennode,aftD[oe]] + G_pri %*% G_beta[,aftD[oe]] +  U + rnorm(N,0,1)
      }
      # pp1 <- exp(pp)/(1+exp(pp))
      # data_ind[,aftD[oe]] <- rbinom(N,1,pp1)
      data_ind[,aftD[oe]] <- pp
    }
  }
  
  
  # for(tty in 1:ncol(data_ind)){
  #   data_ind[,tty] <- data_ind[,tty]+rnorm(N,0,1)
  # }
  
  colnames(data_ind) <- paste0("D",1:D)
  
  #### summary data
  data_sum <- vector("list",D)
  for(k in 1:total_g){
    for(d1 in 1:D){
      fit <- lm(data_ind[,d1]~G_pri[,k])
      data_sum[[d1]] <- rbind(data_sum[[d1]],c(beta=summary(fit)$coef[2,1],
                                               se=summary(fit)$coef[2,2],
                                               pval=summary(fit)$coef[2,4]))
    }
  }
  names(data_sum) <- paste0("D",1:D)
  
  data_sum_beta <- NULL
  for(f in 1:D){
    data_sum_beta <- cbind(data_sum_beta,data_sum[[f]][,1])
    data_sum[[f]] <- as.data.frame(data_sum[[f]])
  }
  
  score <- NULL
  for(d2 in 1:D){
    ed <- (1 + each_g*(d2-1)):(each_g*d2)
    score <- cbind(score,G_pri[,ed] %*% data_sum_beta[ed,d2])
  }
  colnames(score) <- paste0("score_D",1:D)
  
  # colnames(G) <- paste0("SNP",1:ncol(G))
  colnames(G_pri) <- paste0("SNP",1:ncol(G_pri))
  
  return(list(data_ind=cbind(data_ind,G_pri),
              data_sum=data_sum,
              score=score,
              #nmbEdges=nmbEdges,
              D_beta=D_beta,
              G_beta=G_beta,
              D=D))
}





