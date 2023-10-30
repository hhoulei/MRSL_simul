
DataGeneration <- function(N,D,each_g,gl,gu,prob,dlB,duB){
  
  total_g <- D*each_g + each_g
  
  G <- NULL
  for(j in 1:total_g){
    G <- cbind(G,rbinom(N,2,0.3))
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
  D_beta <- matrix(0,nrow=D,ncol=D)
  nmbEdges <- 0L
  for (i in seq_len(D - 2)) {
    listSize <- rbinom(1, D - i, prob)
    nmbEdges <- nmbEdges + listSize
    edgeList <- sample(seq(i + 1, D), size = listSize)
    weightList <- runif(length(edgeList), min = dlB, max = duB)
    
    D_beta[i,edgeList] <- weightList
  }
  
  U <- NULL
  for(ed in 1:D){
    U <- cbind(U,rnorm(N,0,1))
  }
  
  data_ind <- (G %*% G_beta + U) %*% solve(diag(D)-D_beta) 
  
  for(tty in 1:ncol(data_ind)){
    data_ind[,tty] <- data_ind[,tty]+rnorm(N,0,1)
  }
  
  colnames(data_ind) <- paste0("D",1:D)
  
  #### summary data
  data_sum <- vector("list",D)
  for(k in 1:total_g){
    for(d1 in 1:D){
      fit <- lm(data_ind[,d1]~G[,k])
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
    score <- cbind(score,G[,ed] %*% data_sum_beta[ed,d2])
  }
  colnames(score) <- paste0("score_D",1:D)
  
  colnames(G) <- paste0("SNP",1:ncol(G))
  
  return(list(data_ind=cbind(data_ind,G),
              data_sum=data_sum,
              score=score,
              nmbEdges=nmbEdges,
              D_beta=D_beta,
              G_beta=G_beta))
}

