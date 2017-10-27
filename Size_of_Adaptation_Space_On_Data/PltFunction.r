Bnd = as.matrix(read.table('Bandwidth.txt'))
Bnd = Bnd[, 2]
Bnd = split(Bnd, ceiling(seq_along(Bnd)/20))

avg_dist = unlist(lapply(1:length(Bnd), function(i, x)  mean(x[[i]]), Bnd))
s_d = unlist(lapply(1:length(Bnd), function(i, x)  sd(x[[i]]), Bnd))

standard_error = unlist(lapply(1:length(avg_dist), function(i, x) x[i]/sqrt(20), s_d))
x = c(0.25, 0.27, 0.29, 0.31, 0.33, 0.35)
plot(x, avg_dist, pch = 20, col = 'blue', xlab = 'Structural Constraint', ylab = 'Average distance', cex=1.5)
arrows(x, avg_dist - standard_error, x, avg_dist + standard_error, length=0.05, angle=90, code=3, lwd = 2)
