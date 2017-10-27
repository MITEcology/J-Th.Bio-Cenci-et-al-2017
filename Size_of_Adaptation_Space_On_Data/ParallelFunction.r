ReturnVector <- function(i, M, rr, cc, lista_nomi){
  Z <- matrix(BiT_cpp(i, length(rr)*length(cc)), length(rr), length(cc), dimnames = lista_nomi)
  X = SetMatrix(M, Z, rr, cc)
  if (prod(rowSums(X))*prod(colSums(X)) == 0){
    return(1000)
  } else {
    I = create_interaction_matrix(X)
    Y = solve(I) %*% structural_vector(I)
    I = diag(as.vector(Y), nrow(Y)) %*% I
    if(check_stability((I + t(I))/2) == 0){
      return(1000)
    } else{
      rv = structural_vector(create_interaction_matrix(X))
      rv = rv[names(rv) %in% (lista_nomi$col)]
      return(rv)
    }
  }
}

