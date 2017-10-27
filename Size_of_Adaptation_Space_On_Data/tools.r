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
  
  ## Normalisation of the columns ##############################
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

CommonNames <- function(X, Y){
  D = X[rownames(X) %in% rownames(Y), colnames(X) %in% colnames(Y)]
  SubList = list()
  SubList$row = rownames(D)
  SubList$col = colnames(D)
  return(SubList)
}
Non_CommonNames <- function(X, Y){
  D = X[!rownames(X) %in% rownames(Y), !colnames(X) %in% colnames(Y)]
  SubList = list()
  SubList$row = rownames(D)
  SubList$col = colnames(D)
  return(SubList)
}


### Compute the structural constraints
TakeCpp <- function(X, rr, cc){
  X = cnt_cpp(X, rr, cc)
  length_sub = length(rr) * length(cc)
  return(sum(X)/(length(X) - length_sub))
}

