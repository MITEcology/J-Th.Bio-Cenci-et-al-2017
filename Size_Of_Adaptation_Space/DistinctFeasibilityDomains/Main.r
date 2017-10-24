require(compiler)
source('functions.r')
source('tools.r')
require('mgcv')
source('Cfunction.r')
sourceCpp('Cfun.cpp')
require(parallel)
source('PrimaryFunction.r')
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
ptm <- proc.time()
print(format(Sys.time(), "%a %b %d %H:%M:%S %Y"))

sink('Transition.txt')
rr = which(rownames(A) %in% Common_Names$row)
cc = which(colnames(A) %in% Common_Names$col)
val = 1:(realisation - 1)
#### Set the number of cores to use in the simulation
Lavoratori = detectCores() - 1
cl = makeCluster(Lavoratori, type = 'FORK')
StrCtr = TakeCpp(A, rr, cc)
for(link in 0:255){
  v = parLapply(cl, val, parallelizza, A, rr, cc, Common_Names)
  #############################################################
  v = Filter(function(x) length(x) > 1, v) 
  u <- matrix(unlist(v), ncol = n_features, byrow = TRUE)
  NumStableMatrices = nrow(u)
  qr = round(u, 4)
  b = uniquecombs(qr)
  cat(StrCtr, nrow(b), NumStableMatrices, '\n')
  if(link < 15){
    A = Increase_connectance(A, Non_Common_Names, Common_Names, desired = StrCtr)
  } else if((link %% 15) == 0){
    StrCtr = StrCtr + 0.05
    A = Increase_connectance(A, Non_Common_Names, Common_Names, desired = StrCtr)
  } else{
    A = Increase_connectance(A, Non_Common_Names, Common_Names, desired = StrCtr)
  }
  StrCtr = TakeCpp(A, rr, cc)
}
stopCluster(cl)
sink()
print(proc.time() - ptm, '\n')
print(format(Sys.time(), "%a %b %d %H:%M:%S %Y"))
