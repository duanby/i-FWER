# i-FWER
# P:         vector of p-values of length n
# x:         side information 
# alpha:     target FWER level
# mask_fun:  name of the masking function
# mask_para: a vector of parameters in the masking function 
#            eg. [p_l, p_u] for gap function
i_FWER = function(P, x, alpha, mask_fun, mask_para, S_model = TRUE, d = 5, delta = 0.05){
  # generate masked p-values and missing bits
  h_g = mask_gen(P, mask_fun, mask_para); h = h_g$h; g = h_g$g
  
  # start with all the hypotheses in the rejection set
  n = length(P)
  rej_ind = rep(TRUE, n)
  r_neg = sum(h[rej_ind] == -1)
  fwer_est = ifelse(mask_fun %in% c("tent", "railway"),
                    1 - (1 - mask_para)^(r_neg + 1),
                    1 - (1 - mask_para[1]/(mask_para[1] + 1 - mask_para[2]))^(r_neg + 1))
  iter = 0
  while (fwer_est > alpha & any(rej_ind)) {
    masked_P = g*rej_ind + P*(1 - rej_ind)
    if (S_model) { 
      #update S score every 100 iterations
      if (iter %% 100 == 0) {                    
        S = em_mixture(masked_P, x, rej_ind, mask_fun, mask_para)
      }
    } else {
      S = masked_P
    }
    
    rej_ind = exclu_search(S, x, rej_ind, d, delta)
    r_neg = sum(h[rej_ind] == -1)
    fwer_est = ifelse(mask_fun %in% c("tent", "railway"),
                      1 - (1 - mask_para)^(r_neg + 1),
                      1 - (1 - mask_para[1]/(mask_para[1] + 1 - mask_para[2]))^(r_neg + 1))
    iter = iter + 1
  }
  rejections = (rej_ind & h == 1)
  return(rejections)
}



# masked p-value generator
# P:         vector of p-values of length n
# mask_fun:  name of the masking function
# mask_para: a vector of parameters in the masking function
mask_gen = function(P, mask_fun, mask_para){
  if (mask_fun == "tent"){
    h <- 1*(P < mask_para) + (-1)*(P >= mask_para)
    g <- pmin(P, mask_para/(1 - mask_para)*(1 - P))
  } else if (mask_fun == "railway"){
    h <- 1*(P < mask_para) + (-1)*(P >= mask_para)
    g <- P*(P < mask_para) +
      mask_para/(1 - mask_para)*(P - mask_para)*(P >= mask_para) 
  } else if (mask_fun == "gap"){
    h <- 1*(P < mask_para[1]) + (-1)*(P >= mask_para[2])
    g <- P*(P < mask_para[1]) +
      P*(P >= mask_para[1] | P <= mask_para[2]) +
      mask_para[1]/(1 - mask_para[2])*(1 - P)*(P > mask_para[2]) 
  }
  return(list(h = h, g = g))
}



# EM algorithm under mixture model to get the non-null likelihood score
# masked_P:  vector of masked p-value information
# x:         side information 
# rej_ind:   vector of indicators for whether a hypothesis is in the rejection set
# mask_fun:  name of the masking function
# mask_para: a vector of parameters in the masking function
# iter:      number of EM iterations, default to five
em_mixture = function(masked_P, x, rej_ind, mask_fun, mask_para, iter = 5, df = 3){
  masked_P[masked_P == 0] = 10^(-8)  #avoid Inf in calculation
  # translate p-values into Z-scores
  masked_Z = qnorm(1 - masked_P)
  if (mask_fun == "tent") {
    inv_Z = qnorm((1 - mask_para)/mask_para*(1 - pnorm(masked_Z)))*(rej_ind)
  } else if (mask_fun == "railway") {
    inv_Z = qnorm((1 - mask_para)/mask_para*(pnorm(masked_Z) - 1 + mask_para))*(rej_ind) 
  } else if (mask == "gap") {
    inv_Z = qnorm((1 - mask_para[2])/mask_para[1]*(1 - pnorm(masked_Z)))*(rej_ind)
  } 
  
  # initial values for parameters in the EM algorithm
  pi_set = rep(0.1, length(masked_P)); mu = 1
  for (i in 1:iter) {
    mu = mask_para %>% ifelse(mask_fun %in% c("tent", "railway"), ., .[1]) %>%
      max(1, qnorm(1 - .) + 0.5) 
    # likelihood of each case with respect to the latent labels w and q
    a = pi_set*dnorm(masked_Z - mu); b = (1 - pi_set)*dnorm(masked_Z)
    c = pi_set*dnorm(inv_Z - mu); d = (1 - pi_set)*dnorm(inv_Z)
    # update latent labels: w, q
    w = ifelse(rej_ind, 1/(1 + (c + d)/(a + b)), 1)
    q = ifelse(rej_ind, 1/(1 + (b + d)/(a + c)), 1/(1 + b/a))
    # update parameters: pi_set, mu
    phi_x = bs(x[,1], df = df); phi_y = bs(x[,2], df); 
    phi = phi_x[,rep(1:df, each = df)] * phi_y[,rep(1:df, times = df)]
    pi_set = glm(q~phi, family = quasibinomial())$fitted.values
    mu = sum(q*w*masked_Z + q*(1 - w)*inv_Z)/sum(q)
  }
  return(q)
}



# update rejection set under clustered non-null structure
# S:       non-null likelihood score
# x:       side information
# rej_ind: vector of indicators for whether a hypothesis is in the rejection set
# d:       number of cones
# delta:   proportion of hypothesis to be excluded in one cone
exclu_search = function(S, x, rej_ind, d, delta){
  C = colMedians(x[rej_ind,])
  # angle and distance from the center
  theta = atan((x[,1] - C[1]) / (x[,2] - C[2])) * (x[,1] - C[1] >= 0 & x[,2] - C[2] >= 0) +
    (atan((x[,1] - C[1]) / (x[,2] - C[2])) + pi) * (x[,2] - C[2] < 0) +
    (atan((x[,1] - C[1]) / (x[,2] - C[2])) + 2*pi) * (x[,1] - C[1] < 0 & x[,2] - C[2] >= 0)
  square_dist = (x[,1] - C[1])^2 + (x[,2] - C[2])^2
  
  s_min = Inf
  exclu_ind = NULL
  for (j in 1:d) {
    cone_ind = which(theta >= 2*pi/d*(j - 1) & theta < 2*pi/d*j & rej_ind)
    if (length(cone_ind) > 0){
      cand_ind =  order(square_dist[cone_ind], decreasing = TRUE) %>%
        {.[1:round(delta*length(cone_ind))]} %>%
        cone_ind[.]
      s = mean(S[cand_ind])
      if(s < s_min){
        s_min = s
        exclu_ind = cand_ind
      }
    }
  } 
  rej_ind[exclu_ind] = FALSE
  return(rej_ind)
}






