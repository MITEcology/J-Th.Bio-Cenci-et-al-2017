require(compiler)
source('functions.r')
source('tools.r')
require('mgcv')
source('Cfunction.r')
sourceCpp('Cfun.cpp')
require(parallel)
source('FindCentroid.r')
###################################################
#### Generate a binary bipartite interaction network
A = Make_A(0.15)
###################################################
#### Q is here just to sample the common and non common names to use later
Q = Make_Q()
Common_Names = CommonNames(A, Q) 
L = length(Common_Names$row) * length(Common_Names$col)
n_features = length(Common_Names$col)
realisation = 2^L
Non_Common_Names = Non_CommonNames(A, Q)
###############################################
#### Set the original structural constraints of a group
rr = which(rownames(A) %in% Common_Names$row)
cc = which(colnames(A) %in% Common_Names$col)
StrCtr = TakeCpp(A, rr, cc)
#### Count running time
ptm <- proc.time()
print(format(Sys.time(), "%a %b %d %H:%M:%S %Y"))


Vectors = list()
val = 1:(realisation - 1)
#### Choose the number of core you need to use
n_cores = detectCores() - 1
cl = makeCluster(n_cores, type = 'FORK')
##################### Main #######################
for(counter in 1:80){
  v = parLapply(cl, val, parallelizza, A, rr, cc, Common_Names)
  #############################################################
  v = Filter(function(x) length(x) > 1, v) 
  u <- matrix(unlist(v), ncol = n_features, byrow = TRUE)
  qr = round(u, 4)
  b = uniquecombs(qr)
  Vectors[[counter]] = b
  if(counter < 10){
    A = Increase_connectance(A, Non_Common_Names, Common_Names, desired = StrCtr)
    cat(counter, StrCtr)
  } else if((counter %% 10) == 0){
    StrCtr = StrCtr + 0.1
    cat(counter, StrCtr)
    A = Increase_connectance(A, Non_Common_Names, Common_Names, desired = StrCtr)
  } else{
    A = Increase_connectance(A, Non_Common_Names, Common_Names, desired = StrCtr)
    cat(counter, StrCtr)
  }
}
stopCluster(cl)
print(proc.time() - ptm, '\n')
print(format(Sys.time(), "%a %b %d %H:%M:%S %Y"))
save(Vectors, file = 'Vectors.RData')