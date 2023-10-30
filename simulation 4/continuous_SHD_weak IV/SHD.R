SHD  <- function(m1,m2){
  
  shd <- 0
  ## Remove superfluous edges from g1
  s1 <- m1 + t(m1)
  s2 <- m2 + t(m2)
  s1[s1 == 2] <- 1
  s2[s2 == 2] <- 1
  ds <- s1 - s2
  ind <- which(ds > 0)
  m1[ind] <- 0
  shd <- shd + length(ind)/2
  ## Add missing edges to g1
  ind <- which(ds < 0)
  m1[ind] <- m2[ind]
  shd <- shd + length(ind)/2
  ## Compare Orientation
  d <- abs(m1-m2)
  ## return
  SHD <- shd + sum((d + t(d)) > 0)/2
  
  return(SHD)
}