require(compiler)
source('tools.r')
require('mgcv')
source('Cfunction.r')
source('ParallelFunction.r')
source('ToReadData.r')
require(parallel)
sourceCpp('Cfun.cpp')
###################################################
#load("/pathToData/Groups.RData")
File_Name_ = 'data_Menorca.csv'
B = TakeMatrixFromData(File_Name_)$beta
BR = TakeMatrixFromData(File_Name_)$betaR
field = 2
Perturbed = BR[[field]]
Controlled = B[[field]]
########################################
Common_Names = CommonNames(Perturbed, Controlled)
L = length(Common_Names$row) * length(Common_Names$col)
TotalNumberOfSpecies = length(Common_Names$col)
realisation = 2^L
Vectors = list()
#########################################################
ptm <- proc.time()
print(format(Sys.time(), "%a %b %d %H:%M:%S %Y"))
#########################################################
val = 1:(realisation - 1)
Lavoratori = detectCores() - 1
cl = makeCluster(Lavoratori, type = 'FORK')
for(link in 1:length(Nomi)){
  rr = which(rownames(Controlled) %in% Nomi[[link]]$row)
  cc = which(colnames(Controlled) %in% Nomi[[link]]$col)
  cat(link, TakeCpp(Controlled, rr, cc), '\n')
  ###########################################################################
  v = parLapply(cl, val, ReturnVector, Controlled, rr, cc, Nomi[[link]])
  ###########################################################################
  tmp = Filter(function(x) length(x) > 1, v) #### To eliminate the configuration I do not want
  tmp <- matrix(unlist(tmp), ncol = TotalNumberOfSpecies, byrow = TRUE)
  Vectors[[link]] = round(tmp,4)
  ###########################################################################
}
stopCluster(cl)
save(Vectors, file = 'Coordinates.RData')
print(proc.time() - ptm, '\n')
print(format(Sys.time(), "%a %b %d %H:%M:%S %Y"))
