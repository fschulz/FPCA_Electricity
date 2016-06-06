# This function is used to simulate data: See Guo et. al (2015)

f1 = function(x) {
  f1_x = sin(2 * pi * x)/sqrt(0.5)
  return(f1_x)
}
f2 = function(x) {
  f2_x = cos(2 * pi * x)/sqrt(0.5)
  return(f2_x)
}

y_sim = function(x, m, n, sd3, p, s, j) {
  mu_x = 1 + x + exp(-(x - 0.6)^2/0.05)
  if (j == 1) {
    alpha11 = rnorm(m, 0, sqrt(36))   # arima.sim(list(order=c(1,0,0), ar=.3), n=n)
    alpha21 = rnorm(m, 0, sqrt(9))    # arima.sim(list(order=c(1,0,0), ar=.6), n=n)
    alpha1 = alpha11 - mean(alpha11)  # standardize the coefficient to be mean 0. 
    alpha2 = alpha21 - mean(alpha21)
  }
  if (j == 2) {
    Apoly = array(c(-0.5, 0.2, -0.2, 0.5), c(1, 2, 2))
    B     = diag(2)  ## Setting Covariance to identity-matrix
    TRD   = c(0, 0)  ## Setting constant term to 5 and 10
    ## Generating the VAR(2) model
    var2  = ARMA(A = Apoly, B = B, TREND = TRD)
    ## Simulation
    varsim = simulate(var2, sampleT = m, noise = list(w = matrix(rnorm(2 * m), 
      nrow = m, ncol = 2)), rng = list(seed = c(123456)))
    ## Obtaining the generated series
    vardat = matrix(varsim$output, nrow = m, ncol = 2)
    alpha1 = vardat[, 1] - mean(vardat[, 1])
    alpha2 = vardat[, 2] - mean(vardat[, 2])
  }
  y       = rep(0, m * n)
  dim(y)  = c(m, n)
  q       = rep(0, m * n)
  dim(q)  = c(m, n)
  for (i in 1:n) {
    det   = mu_x + f1(x) * alpha1[i] + f2(x) * alpha2[i]
    if (s == 1) {
      e   = rnorm(m, 0, sd = sd3)
      etau= enorm(p, m = 0, sd = sd3)
    }
    if (s == 2) {
      e     = rnorm(m, 0, sd = sd3 * mu_x[i])  # expectile
      etau  = enorm(p, m = 0, sd = sd3 * mu_x[i])
    }
    if (s == 3) {
      e     = rt(m, 5)  # expectile
      etau  = et(p, 5)
    }
    q[, i]  = det + etau
    y[, i]  = det + e
  }
  list(mu_x = mu_x, f1_x = f1(x), f2_x = f2(x), alpha1 = alpha1, alpha2 = alpha2, 
    y = y, q = q)
}

 
