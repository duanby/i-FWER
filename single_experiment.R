single_experiment = function(para_vary){
  source("input.R", local = TRUE)
  for (single_para_vary in para_vary) {
    assign(single_para_vary$name, single_para_vary$value)
  }
  
  rejections = list()
  power = matrix(ncol = length(methods), nrow = R); colnames(power) = methods
  error = matrix(ncol = length(methods), nrow = R); colnames(error) = methods
  for (i in 1:R) {
    print(i)
    nonnull_ind = cluster_nonnull(D = D, d = d)
    dat = sim_dat(nonnull_ind, mu_1 = mu_1, mu_0 = mu_0)
    
    rejections[[i]] = list()
    if ("Sidak" %in% methods) {
      rejections[[i]][["Sidak"]] = (dat$P < 1 - (1 - alpha)^(1/D^2))
    }
    if ("i-fwer-tent" %in% methods) {
      rejections[[i]][["i-fwer-tent"]] = i_FWER(dat$P, dat$x, alpha,
                                          mask_fun = "tent", mask_para = pstar, S_model = smoothed)
    }
    if ("i-fwer-railway" %in% methods) {
      rejections[[i]][["i-fwer-railway"]] = i_FWER(dat$P, dat$x, alpha,
                                             mask_fun = "railway", mask_para = pstar, S_model = smoothed)
    }
    if ("i-fwer-gap" %in% methods) {
      rejections[[i]][["i-fwer-gap"]] = i_FWER(dat$P, dat$x, alpha,
                                         mask_fun = "gap", mask_para = c(p_l, p_u), S_model = smoothed)
    }
    
    power[i,] = sapply(rejections[[i]], function(x) {sum(nonnull_ind & x)/sum(nonnull_ind)}) 
    error[i,] = sapply(rejections[[i]], function(x) {any(!nonnull_ind & x)})
  }
  return(list(rejections = rejections, power = power, error = error))
}