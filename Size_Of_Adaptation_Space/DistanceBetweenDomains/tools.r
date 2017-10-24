Rescaled_Gram_Matrix <- function(Y){
  beta <- t(Y) %*% Y
  S <- nrow(beta)
  for (j in 1:ncol(beta)){
    beta[,j] = beta[,j]/colSums(beta)[j]
  }
  beta = beta - diag(diag(beta)) + diag(1,S)
  return(beta)
}

create_interaction_matrix <- function(A){
  B <- Rescaled_Gram_Matrix(A) 
  
  ## Normalisation of the columns in the L2 norm ###############
  for (k in 1:ncol(B)){                                      
    B[, k] <- B[, k]/sqrt(sum(B[,k]^2))
  }
  ##############################################################
  return(B)
}

check_stability <- function(O){
  
  tmp <- eigen(O)
  w <- tmp$values
  check <- sum(Re(w) < 0) ## if there are negative eigenvalues then check > 0
  if (check > 0){
    # Not stable
    return (0)
  }
  else {
    return(1)
  }
}


structural_vector <- function(Sigma){
  #D <- diag(1/sqrt(diag(t(Sigma) %*% Sigma))) ## pointless if the columns are normalised in the L2 norm
  beta2 <- Sigma # %*% D
  S <- nrow(beta2)
  alpha_s <- rowSums(beta2)/S
  alpha_s <- alpha_s/sqrt(sum(alpha_s^2))

  K <- alpha_s
  return(K)
}

calculate_deviation <- function(K, KS){
 #Calculate the angle between the eigenvectors
  cos_theta <- (t(K) %*% KS) / (sqrt(t(K) %*% K) * sqrt(t(KS) %*% KS))
  return(acos(pmin(pmax(cos_theta,-1.0),1.0)))
}

require(mvtnorm) #this function requires the librrary mvtnorm (Genz et al. 2009) for the
#numerical computation of the cumulative distribution of the multivariate normal distirbution

Omega <- function(alpha){
  ### Volume of the feasibility domain as the value of the multivariate gamma distirbution
  ### with variance-covariance matrix Gamma = (Sigma^TSigma)^-1
  S <- nrow(alpha)
  Gamma <-solve(t(alpha) %*% alpha)
  m <- matrix(0,S,1)
  a <- matrix(0,S,1)
  b <- matrix(Inf,S,1)  
  d <- pmvnorm(lower = rep(0,S), upper = rep(Inf,S), mean = rep(0,S), sigma = Gamma)
  out <- d[1]*2^(nrow(alpha)) #log10(d[1]) # + S * log10(2)
  return(out)
}


