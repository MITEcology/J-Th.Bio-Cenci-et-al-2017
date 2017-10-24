Connectance <- function(A){
  return(sum(A)/length(A))
}

CommonNames <- function(X, Y){
  C = X[rowSums(X) != 0,colSums(X) != 0]
  D = Y[rowSums(Y) != 0,colSums(Y) != 0]
  if(min(length(C), length(D)) == length(C)){
    Mn = C
    Mx = D
  } else{ Mn = D; Mx = C  }
  K = Mn[rownames(Mn) %in% rownames(Mx), colnames(Mn) %in% colnames(Mx)]
  SubList = list()
  SubList$row = rownames(K)
  SubList$col = colnames(K)
  return(SubList)
}

Non_CommonNames <- function(X, Y){
  C = X[rowSums(X) == 0,colSums(X) == 0]
  D = Y[rowSums(Y) == 0,colSums(Y) == 0]
  SubList = list()
  SubList$row = c(as.vector(rownames(C)), as.vector(rownames(D)))
  SubList$col = c(as.vector(colnames(C)), as.vector(colnames(D)))
  return(SubList)
}



Sub_System<- function(X, N_list){
  X = X[rownames(X) %in% N_list$row, colnames(X) %in% N_list$col]
  return(list(Matrix = X, Conn = Connectance(X)))
}

TakeCpp <- function(X, rr, cc){
  X = cnt_cpp(X, rr, cc)
  length_sub = length(rr) * length(cc)
  return(sum(X)/(length(X) - length_sub))
}



TakeStructuralConstraints <- function(X, lista_nomi_comuni){
  for(i in lista_nomi_comuni$row){
    for(j in lista_nomi_comuni$col){
      X[i,j] = 0
    }
  }
  length_sub = length(lista_nomi_comuni$row) * length(lista_nomi_comuni$col)
  return(sum(X)/(length(X) - length_sub))
}

countPossibilities <- function(q, n){
  k = w = 0
  for(i in 2:(q-1)){
    w = 0
    for(j in 2:(n-1)){
      w = w + choose(n,j)
    }
    k = k + choose(q, i)*w
  }  
  return(k)
}

ChooseCombination <- function(X, a, b){
  #### Give the list of common names. Choose a unique rows and b unique columns
  x = list()
  exit = 0
  while(exit == 0){
    x$row = sample(X$row, a)
    x$col = sample(X$col, b)
    x$row = unique(x$row)
    x$col = unique(x$col)
    if(length(x$row == a) && length(x$col == b)){
      exit = 1
    }
  }
  return(x)
}

################### To avoi double loop

f2 <- function(i, a, b, lt, n){
  lt = ChooseCombination(n, a[i,1], a[i, 2])
  return(TakeConnectance(b, lt))
}
f1 <- function(i, a, b, lt, n){
  s = lapply(1:nrow(d), f2, a, b, lt, n)
  return(s)
}


