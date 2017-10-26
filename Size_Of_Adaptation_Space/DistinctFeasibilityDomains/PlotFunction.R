Tr = as.matrix(read.table('Transition.txt'))
Tr = Tr[-1,]
StrCtr = Tr[, 1]
Fraction_Distinct = Tr[, 2]
realisation = 15 ### Number of realisations for each data point
max_number_Configurations = Tr[,3]
StrCtr = split(StrCtr, ceiling(seq_along(StrCtr)/realisation))
Fraction_Distinct = split(Fraction_Distinct, ceiling(seq_along(Fraction_Distinct)/realisation))
numb_Max_conf = split(max_number_Configurations, ceiling(seq_along(max_number_Configurations)/realisation))


StrCtr = unlist(lapply(1:length(StrCtr), function(i, x)  mean(x[[i]]), StrCtr))
Fraction_Distinct = unlist(lapply(1:length(Fraction_Distinct), function(i, x)  mean(x[[i]])/mean(numb_Max_conf[[i]]), Fraction_Distinct))

plot(StrCtr, Fraction_Distinct, pch = 20, col = 'blue', xlab = 'Structural Constraint', ylab = 'Fraction of Distinct Domains', cex=1.5)

