source('fun.r')
require(Rcpp)
sourceCpp('Cfun.cpp')
source('ToReadData.r')
require(parallel)
source('SamplingFunctions.r')

### Here instead of computing the structural constraints of all group you randomly sample them
#####################################
### Field to study
field = 3
### Read the data
### ### Download the data at http://datadryad.org/resource/doi:10.5061/dryad.77rn2
#File_Name_ = 'data_Menorca.csv'
B = TakeMatrixFromData(File_Name_)$beta
Controlled = B[[field]]
BR = TakeMatrixFromData(File_Name_)$betaR
Perturbed = BR[[field]]


Common_Names = CommonNames(Perturbed, Controlled) 

### Number of group to sample
values = 1:8e6
### number of core to use for the parallel computation
Lavoratori = detectCores() - 1
cl <- makeCluster(Lavoratori, type = "FORK")
res <- parLapply(cl, values, RandomSampling, Common_Names, Controlled)
stopCluster(cl)


StrCtr = unlist(res)
StrCtr_emp = TakeStructuralConstraints(Controlled, Common_Names)
h = hist(StrCtr, breaks = 100)
abline(v = StrCtr_emp, col = 'blue')
################################################
################################################

