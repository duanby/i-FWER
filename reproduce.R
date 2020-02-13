source("setup.R")
############### Figure 5 ###############
# compare i-FWER(-tent) with Sidak with varying alternative mean
mu_1_seq = 1:5

# grid size 10*10
result_grid10 = list()
for (i in 1:length(mu_1_seq)) {
  para_vary = list(list(name = "mu_1", value = mu_1_seq[i]), list(name = "D", value = 10))
  result_grid10[[as.character(mu_1_seq[i])]] = single_experiment(para_vary)
}
save(result_grid10, file="result/tent-sidak-10.Rdata")
print(sapply(result_grid10, function(x){colMeans(x$power)}))
# grid size 30*30
result_grid30 = list()
for (i in 1:length(mu_1_seq)) {
  para_vary = list(list(name = "mu_1", value = mu_1_seq[i]))
  result_grid30[[i]] = single_experiment(para_vary)
}
save(result_grid30, file="result/tent-sidak-30.Rdata")
print(sapply(result_grid30, function(x){colMeans(x$power)}))
if (0){
  # plot result
  aver_power = rbind(sapply(result_grid10, function(x){colMeans(x$power)}), sapply(result_grid30, function(x){colMeans(x$power)}))
  df_power = data.frame(mu_seq = rep(mu_1_seq, each = 4),
                        power = as.vector(aver_power),
                        grp = rep(c("Sidak", "i-FWER-tent", "Sidak", "i-FWER-tent"), length(mu_1_seq)))
  p = ggplot(data = df_power,
             aes(x=mu_1_seq, y = power, group = grp, fill = grp)) +
    geom_line(aes(linetype=grp, color = grp), size = 0.8) +
    geom_point(aes(shape=grp, color = grp), size = 2.5) +
    scale_linetype_manual(values=c("solid", "solid", "dashed", "dashed")) +
    scale_shape_manual(values=c(22, 21, 24)) +
    scale_color_manual(values=c("#00FF00", "#0099FF", "FF9900", "#FF3366")) +
    scale_fill_manual(values=c("#00FF00", "#0099FF", "FF9900", "#FF3366")) +
    theme(legend.title = element_blank(),
          panel.background = element_rect(fill = "white", colour="black"),
          panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
          panel.grid.minor = element_line(colour = "grey"),
          text = element_text(size = 15),
          legend.position = c(0.75, 0.65), legend.text = element_text(size=12))+
    xlab("alternative mean") + ylab("power") +
    scale_y_continuous(breaks=seq(0,1,0.2), limits= c(0,1))
  plot(p)
  ggsave(filename = "figure/tent-sidak.pdf", plot = p, device = "pdf", width = 4, height = 3.6)
}


