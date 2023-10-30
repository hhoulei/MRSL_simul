
ff <- NULL
rr <- NULL
for(oi in 1:1000){
  N=10000
  g=20
  G <- NULL
  for(i in 1:g){
    G <- cbind(G,rbinom(N,2,0.3))
  }
  gl=0.03
  gu=0.14
  alpha <- c(runif(g-16,gl,gu),rep(0.001,16))
  X <- G %*% alpha + rnorm(N,0,1)
  
  fit <- lm(X~G)
  ff <- c(ff,summary(fit)$fstatistic[1])
  rr <- c(rr,summary(fit)$r.squared)
}

mean(rr)

# 0.1, 0.061%
# 0.3, 0.048%
# 0.5, 0.035%
# 0.8, 0.015%


