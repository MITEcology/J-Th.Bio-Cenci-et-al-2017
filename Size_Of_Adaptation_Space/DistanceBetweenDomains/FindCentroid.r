parallelizza <- function(i, A, rr, cc, Common_Names){
  Z <- matrix(BiT_cpp(i, length(rr)*length(cc)), length(rr), length(cc), dimnames = Common_Names)
  A = SetMatrix(A, Z, rr, cc)
  if (prod(rowSums(A))*prod(colSums(A)) == 0){
    return(0)
  } else {
    I = create_interaction_matrix(A)
    if(check_stability((I + t(I))/2) == 0){
      return(0)
    } else{
      rv = structural_vector(I)
      rv = rv[names(rv) %in% Common_Names$col]
      return(rv)
    }
  }
}
