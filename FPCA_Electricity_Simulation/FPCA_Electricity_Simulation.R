
libraries = c("fda.usc", "fda", "car", "tseries", "quantreg", "orthogonalsplinebasis", 
  "expectreg", "splines", "Matrix", "MatrixModels", "cobs", "fields", "vars", "dse")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, require, quietly = TRUE, character.only = TRUE)

#setwd("...")
# load functions
source("FPCA_Electricity_simulation_setup.R")

# set parameters

M       = 100  # number of obs per curve
N       = 20  # number of curves
K       = 200
meanMSE = matrix(ncol = 1, nrow = K)
meanSD  = matrix(ncol = 1, nrow = K)
rt      = matrix(ncol = 1, nrow = K)

for (k in 1:K) {
  
  xsim  = seq(1/M, 1, length.out = M)  # generate x
  p     = 0.95  # calculated expectile
  d3    = sqrt(0.5)
  t     = 1:M
  
  # get simulated data
  set.seed  = 123 + k
  data.sim  = y_sim(xsim, M, N, d3, p, 1, 1)  ## chose one of the 3 error distributions and eiter independent (1) or autocorrelated (2) residuals
  mu_x      = data.sim[[1]]
  f1_x      = data.sim[[2]]
  f2_x      = data.sim[[3]]
  alpha1    = data.sim[[4]]
  alpha2    = data.sim[[5]]
  ysim      = data.sim[[6]]
  q         = data.sim[[7]]
  
  ## expectile regression
  ptm = proc.time()
  # expectiles
  fit = matrix(ncol = N, nrow = M)
  for (i in 1:N) {
    y         = expectreg.ls(ysim[, i] ~ rb(xsim, "pspline", center = FALSE), estimate = "laws", 
                expectiles = p, smooth = "gcv")  # ci=TRUE
    fit[, i]  = y$fitted
  }
  data = Data2fd(xsim, fit)
  
  # FPCA
  PCA   = pca.fd(data, nharm = 2, centerfns = TRUE)
  PC    = PCA$harmonics
  mean  = PCA$meanfd
  a     = PCA$scores
  PC1   = PC[1]
  PC2   = PC[2]
  
  
  # reconstruct curves using Karhunen-Loeve
  
  est = matrix(ncol = N, nrow = M)
  MSE = matrix(ncol = N, nrow = M)
  for (i in 1:N) {
    est[, i] = eval.fd(xsim, (mean + PC1 * a[i, 1] + PC2 * a[i, 2]))
    MSE[, i] = ((q[, i] - est[, i])^2)
  }
  
  time  = (proc.time() - ptm)
  rt[k] = time[3]/K
}

m = mean(colMeans(MSE))
s = sd(colMeans(MSE)) 