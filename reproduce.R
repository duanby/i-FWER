source("setup.R")
############### Figure 5 ###############
# compare i-FWER(-tent) with Sidak when varying alternative mean
mu_1_seq = 1:5

# grid size 10*10
result_grid10 = list()
for (i in 1:length(mu_1_seq)) {
  para_vary = list(list(name = "mu_1", value = mu_1_seq[i]),
                   list(name = "D", value = 10))
  result_grid10[[as.character(mu_1_seq[i])]] = single_experiment(para_vary)
}
save(result_grid10, file="result/tent-sidak-10.Rdata")

# grid size 30*30
result_grid30 = list()
for (i in 1:length(mu_1_seq)) {
  para_vary = list(list(name = "mu_1", value = mu_1_seq[i]))
  result_grid30[[i]] = single_experiment(para_vary)
}
save(result_grid30, file="result/tent-sidak-30.Rdata")



############### Figure 7 ###############
# compare i-FWER-tent with i-FWER-railway when varying null mean
mu_0_seq = 0:-4
result_railway = list()
for (i in 1:length(mu_0_seq)) {
  para_vary = list(list(name = "mu_0", value = mu_0_seq[i]),
                   list(name = "methods", value = c("Sidak", "i-fwer-tent", "i-fwer-railway")),
                   list(name = "smoothed", value = FALSE))
  result_railway[[as.character(mu_0_seq[i])]] = single_experiment(para_vary)
}
save(result_railway, file = "result/tent-railway.Rdata")


############### Figure 8 ###############
# compare i-FWER-tent with i-FWER-railway when varying null mean
mu_1_seq = 1:5
result_gap = list()
for (i in 1:length(mu_1_seq)) {
  para_vary = list(list(name = "mu_1", value = mu_1_seq[i]),
                   list(name = "methods", value = c("Sidak", "i-fwer-tent", "i-fwer-gap")))
  result_gap[[as.character(mu_1_seq[i])]] = single_experiment(para_vary)
}
save(result_gap, file = "result/tent-gap.Rdata")

