parallelizza <- function(i, A, rr, cc, Common_Names){
  Z <- matrix(BiT_cpp(i, length(rr)*length(cc)), length(rr), length(cc), dimnames = Common_Names)
  A = SetMatrix(A, Z, rr, cc)
  if (prod(rowSums(A))*prod(colSums(A)) == 0){
    return(0)
  } else {
    ### Check for the global stabiltiy of the fixed point
    I = create_interaction_matrix(A)
    X = solve(I) %*% structural_vector(I)
    I = diag(as.vector(X), nrow(X)) %*% I
    if(check_stability((I + t(I))/2) == 0){
      return(0)
    } else{
      ### If it is globally stable then OK
      rv = structural_vector(create_interaction_matrix(A))
      rv = rv[names(rv) %in% Common_Names$col]
      return(rv)
    }
  }
}
