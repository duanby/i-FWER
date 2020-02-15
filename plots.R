source("setup.R")
colSes = function(m) {apply(m, 2, sd)/sqrt(nrow(m))} #standard error
dir.create("figure")
######################################################
##################### Figure 5 #######################
load(file = "result/tent-sidak-10.Rdata")
load(file = "result/tent-sidak-30.Rdata")

## power plot
mean_power = rbind(sapply(result_grid10, function(x){colMeans(x$power)}),
                   sapply(result_grid30, function(x){colMeans(x$power)}))
sd_power = rbind(sapply(result_grid10, function(x){colSes(x$power)}),
                 sapply(result_grid30, function(x){colSes(x$power)}))
df_power = data.frame(mu_seq = rep(as.numeric(names(result_grid10)), each = 4),
                      power = as.vector(mean_power),
                      grp = rep(c("Sidak 10*10", "i-FWER 10*10", "Sidak 30*30", "i-FWER 30*30"),
                                length(names(result_grid10))))
p = ggplot(data = df_power,
           aes(x = mu_seq, y = power, group = grp, fill = grp)) +
  geom_line(aes(linetype = grp, color = grp), size = 0.8) +
  geom_point(aes(shape = grp, color = grp), size = 2.5) +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed")) +
  scale_shape_manual(values = c(22, 21, 25, 24)) +
  scale_color_manual(values = c("#00FF00", "#0099FF", "#FF9900", "#FF3366")) +
  scale_fill_manual(values = c("#00FF00", "#0099FF", "#FF9900", "#FF3366")) +
  theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "grey"),
        text = element_text(size = 15),
        legend.position = c(0.2, 0.8), legend.text = element_text(size = 10)) +
  xlab("alternative mean") + ylab("power") +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))
plot(p)
ggsave(filename = "figure/tent-sidak-power.pdf", plot = p, device = "pdf", width = 4, height = 3.6)

## error plot
mean_error = rbind(sapply(result_grid10, function(x){colMeans(x$error)}),
                   sapply(result_grid30, function(x){colMeans(x$error)}))
sd_error = rbind(sapply(result_grid10, function(x){colSes(x$error)}),
                 sapply(result_grid30, function(x){colSes(x$error)}))
df_error = data.frame(mu_seq = rep(as.numeric(names(result_grid10)), each = 4),
                      error = as.vector(mean_error),
                      grp = rep(c("Sidak 10*10", "i-FWER 10*10", "Sidak 30*30", "i-FWER 30*30"),
                                length(names(result_grid10))))
p = ggplot(data = df_error,
           aes(x = mu_seq, y = error, group = grp, fill = grp)) +
  geom_line(aes(linetype = grp, color = grp), size = 0.8) +
  geom_point(aes(shape = grp, color = grp), size = 2.5) +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed")) +
  scale_shape_manual(values = c(22, 21, 25, 24)) +
  scale_color_manual(values = c("#00FF00", "#0099FF", "#FF9900", "#FF3366")) +
  scale_fill_manual(values = c("#00FF00", "#0099FF", "#FF9900", "#FF3366")) +
  theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "grey"),
        text = element_text(size = 15),
        legend.position = c(0.2, 0.8), legend.text = element_text(size = 10)) +
  xlab("alternative mean") + ylab("FWER") +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))
plot(p)
ggsave(filename = "figure/tent-sidak-error.pdf", plot = p, device = "pdf", width = 4, height = 3.6)



######################################################
##################### Figure 7 #######################
load(file = "result/tent-railway.Rdata")

mean_power = sapply(result_railway, function(x){colMeans(x$power)})
sd_power = sapply(result_railway, function(x){colSes(x$power)})
df_power = data.frame(mu_seq = rep(-as.numeric(names(result_railway)), each = 3),
                      power = as.vector(mean_power),
                      grp = rep(c("Sidak", "i-FWER-tent", "i-FWER-railway"),
                                length(names(result_railway))))
p = ggplot(data = df_power,
           aes(x = mu_seq, y = power, group = grp, fill = grp)) +
  geom_line(aes(linetype = grp, color = grp), size = 0.8) +
  geom_point(aes(shape = grp, color = grp), size = 2.5) +
  scale_linetype_manual(values = c("twodash", "solid", "dashed")) +
  scale_shape_manual(values = c(22, 21, 24)) +
  scale_color_manual(values = c("#00FF00", "#0099FF", "#FF3366")) +
  scale_fill_manual(values = c("#00FF00", "#0099FF", "#FF3366")) +
  theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "grey"),
        text = element_text(size = 15),
        legend.position = c(0.75, 0.65), legend.text = element_text(size = 12)) +
  xlab("null mean") + ylab("power") +
  scale_y_continuous(breaks=seq(0,1,0.2), limits = c(0,1)) + scale_x_continuous(labels = -0:-4)
plot(p)
ggsave(filename = "figure/tent-railway.pdf", plot = p, device = "pdf", width = 4, height = 3.6)



######################################################
##################### Figure 8 #######################
load(file = "result/tent-gap.Rdata")

mean_power = sapply(result_gap, function(x){colMeans(x$power)})
sd_power = sapply(result_gap, function(x){colSes(x$power)})
df_power = data.frame(mu_seq = rep(as.numeric(names(result_gap)), each = 3),
                      power = as.vector(mean_power),
                      grp = rep(c("Sidak", "i-FWER-tent", "i-FWER-gap"), length(names(result_gap))))
p = ggplot(data = df_power,
           aes(x = mu_seq, y = power, group = grp, fill = grp)) +
  geom_line(aes(linetype = grp, color = grp), size = 0.8) +
  geom_point(aes(shape = grp, color = grp), size = 2.5) +
  scale_linetype_manual(values = c("twodash", "solid", "dashed")) +
  scale_shape_manual(values = c(22, 21, 24)) +
  scale_color_manual(values = c("#FF9900", "#0099FF", "#FF3366")) +
  scale_fill_manual(values = c("#FF9900", "#0099FF", "#FF3366")) +
  theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "grey"),
        text = element_text(size = 15),
        legend.position = c(0.22, 0.85), legend.text = element_text(size = 12)) +
  xlab("alternative mean") + ylab("power") +
  scale_y_continuous(breaks=seq(0,1,0.2), limits = c(0,1))
plot(p)
ggsave(filename = "figure/tent-gap.pdf", plot = p, device = "pdf", width = 4, height = 3.6)


