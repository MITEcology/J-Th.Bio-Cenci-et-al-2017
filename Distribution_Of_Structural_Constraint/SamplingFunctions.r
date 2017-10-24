compute_in_parallel <- function(q_m, CN){
  R = combn(rownames(q_m), length(CN$row))
  C = combn(colnames(q_m), length(CN$col))
  values = 1:ncol(R)
  Lavoratori = detectCores() - 1
  cl <- makeCluster(Lavoratori, type = "FORK")
  res <- parLapply(cl, values, F1, q_m, R, C)
  stopCluster(cl)
  return(res)
}

F1 <- function(i, q_m, R, C){
  value = 1:ncol(C)
  g <- lapply(value, F2, q_m, R, C, i)
  return(g)
}

F2 <- function(i, q_m, R, C, Vl){
  rr = which(rownames(q_m) %in% R[, Vl])
  cc = which(colnames(q_m) %in% C[, i])
  return(TakeCpp(q_m, rr, cc))
}
###############################
RandomSampling <- function(i, cmb, q_m){
  R = sample(rownames(q_m), length(cmb$row))
  C = sample(colnames(q_m), length(cmb$col))
  rr = which(rownames(q_m) %in% R)
  cc = which(colnames(q_m) %in% C)
  return(TakeCpp(q_m, rr, cc))
}


