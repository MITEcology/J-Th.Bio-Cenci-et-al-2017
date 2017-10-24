Connectance <- function(A){
  return(sum(A)/length(A))
}

Make_Q <- function(rown = 6, coln = 9){
  NaM <- list()
  NaM$row <- c('a', 'b', 'c', 'd', 'e', 'o')
  NaM$col <- c('a1', 'a2', 'a3', 'a4', 'a5', 'b1', 'b2', 'b3', 'Q1')
  esci = 0
  while(esci == 0){
    X <- matrix(1, rown, coln, dimnames = NaM)
    C = Connectance(X)
    while(C > 0.4){
      X[sample(rown, 1), sample(coln, 1)] = 0
      C = Connectance(X)
    }
    if (prod(rowSums(X))*prod(colSums(X)) == 0){
    }else{esci = 1}}
  return(X)
}

###################################################################
Make_A <- function(cnt = 0.3){
  
  NaM <- list()
  NaM$row <- c('a', 'b', 'D', 'L', 'e', 'J', 'Q', 'Y', 'G', 's', 'u', 'm', 'k', 'q', 'y', 'k11', 'k22', 'k44', 'k33', 'k55', 'k01', 'k02', 'k03', 'k04')
  NaM$col <- c('a2', 'b3', 'L4', 'a5', 'a3', 'K1', 'Z4', 'Z1', 'a1', 'b2', 'R3', 'R4', 'R5', 'R6', 'T7', 'L1', 'M1', 'S1', 'H22', 'H33', 'H44', 'H55', 'H66', 'H01', 'H02', 'H03', 'H04', 'H08')
  esci = 0
  rown = length(NaM$row)
  coln = length(NaM$col)
  L = rown * coln
  while(esci == 0){
    A <- matrix(1, rown, coln, dimnames = NaM)
    C = Connectance(A)
    while(C > cnt){
      A[sample(rown, 1), sample(coln, 1)] = 0
      C = Connectance(A)
    }
    if (prod(rowSums(A))*prod(colSums(A)) == 0){
    }else{esci = 1}}
  return(A)
}
###################################################################
Increase_connectance <- function(X, NC_list, C_list, desired = 0.6, RightNoise = 0){
  X[X != 0] = 0
  Core_size = length(C_list$row) * length(C_list$col)
  MaxSumGivenCore = length(X) - Core_size
  C = sum(X)/MaxSumGivenCore
  while(RightNoise == 0){
    X[X != 0] = 0
    C = sum(X)/MaxSumGivenCore
    while(C < desired){
      r = runif(1, 0, 1)
      if(r < 0.33){
        X[sample(NC_list$row, 1),sample(NC_list$col, 1)] = 1
      } else if(r < 0.66){
        X[sample(NC_list$row, 1),sample(C_list$col, 1)] = 1
      }else{
        X[sample(C_list$row, 1),sample(NC_list$col, 1)] = 1
      }
      C = sum(X)/MaxSumGivenCore
    }
    NC_Matrix = X[!rownames(X) %in% C_list$row, !colnames(X) %in% C_list$col]
    if(prod(rowSums(NC_Matrix))*prod(colSums(NC_Matrix)) == 0){
    } else{RightNoise = 1}}
  return(X)
}
####################################################
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
#########################

TakeCpp <- function(X, rr, cc){
  X = cnt_cpp(X, rr, cc)
  length_sub = length(rr) * length(cc)
  return(sum(X)/(length(X) - length_sub))
}

