# simulate a vector p-values and side information
# nonnull_ind: a grid of non-null indicators
# mu_1:        non-null mean value
# mu_0:        null mean value
sim_dat = function(nonnull_ind, mu_1, mu_0){
  n_col = ncol(nonnull_ind); n_row = nrow(nonnull_ind)
  x <- cbind(rep(1:n_col, each = n_row), rep(1:n_row, times = n_col))
  
  mu <- as.vector(nonnull_ind)*mu_1 + as.vector(!nonnull_ind)*mu_0
  dat <- rnorm(n_col*n_row) + mu
  P <- 1 - pnorm(dat)
  return(list(P = as.vector(P), x = x))
}



# generate the a grid of non-null indicators with non-nulls clustered
# D: size of grid D*D
# d: size of non-null cluster (= radius^2)
cluster_nonnull = function(D, d){
  nonnull_ind = matrix(FALSE, ncol = D, nrow = D)
  C = c(D/2, D/2)
  bd_x <- floor(sqrt(d))
  for (i in (C[1] - bd_x):(C[1] + bd_x)) {
    bd_y <- floor(sqrt(d - (C[1] - i)^2))
    for (j in (C[2] - bd_y):(C[2] + bd_y)) {
      nonnull_ind[i,j] = TRUE
    }
  }
  return(nonnull_ind)
}





