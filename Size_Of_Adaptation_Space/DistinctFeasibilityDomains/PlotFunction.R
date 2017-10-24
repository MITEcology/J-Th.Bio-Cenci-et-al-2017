a = as.matrix(read.table('Transition.txt'))
cnt = rnd4 = c()
ct = r4 = NM = 0
realisation = 15 ### Number of realisations for each outer-connectance
for(i in 1:nrow(a)){
  ct = ct + a[i, 1]
  r4 = r4 + a[i, 2]
  NM = NM + a[i, 3]
  if((i %% realisation) == 0){
    cnt = c(cnt, ct/realisation)
    r4 = r4/realisation
    NM = NM/realisation
    rnd4 = c(rnd4, r4/NM) ### Divide by the number of stable matrices
    ct = r4 = NM = 0
  }
}

plot(cnt, rnd4, col = 'red')
