### Likelihood estimates for Hyper()

tab1 <- matrix(rep(NA, 6*13), 6, 13)
tab1
for(i in 0:5) {
  for(j in 0:12) {
    tab1[i+1,j+1] <- dhyper(i, j, 12 - j, 5)
  }
}

tab1

names(tab1)

