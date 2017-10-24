require('compiler')
source('fun.r')
source('ToReadData.r')
require(Rcpp)
sourceCpp('Cfun.cpp')
require(parallel)
source('SamplingFunctions.r')

### Field to study (1, 2 or 3). For field three we advise to sample randomly the groups as 
### The network is large
field = 1
### Reference network
### Download the data at http://datadryad.org/resource/doi:10.5061/dryad.77rn2
#File_Name_ = 'data_Menorca.csv'
B = TakeMatrixFromData(File_Name_)$beta
Controlled = B[[field]]
### perturbed network 
BR = TakeMatrixFromData(File_Name_)$betaR
Perturbed = BR[[field]]

### Find the species common to both Fields
Common_Names = CommonNames(Perturbed, Controlled) 

### Compute the structural constraints of all groups with same dimension
### Note that here I am using all but one core
s = compute_in_parallel(Controlled, Common_Names)
StrCtr = unlist(s)
StrCtr_ob = TakeStructuralConstraints(Controlled, Common_Names)
h = hist(StrCtr, breaks = 20)
abline(v = StrCtr_ob, col = 'red')
####################################
####################################
####################################
