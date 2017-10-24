dta = as.matrix(read.table('Bandwidth.txt'))
## Take only the bandwith
dta = dta[,2]
### This is the number of times you average over. 
### It is equal to the number of samples with same structural constraint
n_realization = 10
#### Initialize
bndw = c()
bd  =  0
for(i in 1:length(dta)){
  bd = bd + dta[i]
  if((i %% n_realization) == 0){
    bndw = c(bndw, bd/n_realization)
    bd = 0
  }
}

### here is just an example of the x-axis
x = c(0.13,0.23,0.33,0.43,0.53,0.63,0.73,0.83)
plot(x, bndw, xlab = 'structural constraint', ylab = 'distance', main = 'Size of the Adaptation Space', col = 'blue')
#### Print on a txt file
sink('FinalResults.txt')
for(i in 1:length(bndw)){
  cat(x[i], bndw[i], '\n')
}
sink()