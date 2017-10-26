dta = as.matrix(read.table('Bandwidth.txt'))
## Take only the bandwith
dta = dta[,2]
### This is the number of times you average over. 
### It is equal to the number of samples with same structural constraint
realisation = 10
bndw = split(dta, ceiling(seq_along(dta)/realisation))
bndw = unlist(lapply(1:length(bndw), function(i, x)  mean(x[[i]]), bndw))
### Here is just an example of the x-axis (i.e., the one we have use to generate the data)
x = c(0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85)
plot(x, bndw, pch = 20, xlab = 'structural constraint', ylab = 'distance', main = 'Size of the Adaptation Space', col = 'blue', cex=1.5)
