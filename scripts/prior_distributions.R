library(ggplot2)
common_ops <-  list(theme(plot.title = element_text(size=8),
                          axis.text = element_text(size = 8),
                          axis.title = element_text(size = 8),
                          strip.text = element_text(size = 8),
                          legend.position="bottom",
                          legend.key = element_rect(size = 5),
                          legend.key.size = unit(1.5, 'lines'),
                          #   panel.grid.major=element_blank(),
                          #   panel.grid.minor=element_blank(),
                          panel.border=element_blank(),
                          axis.line=element_line(),
                          text=element_text()))
prior_dist <- ggplot() +
  geom_line(aes(x = seq(-10, 10, .01), y = dunif(seq(-10, 10,.01),-10,10), colour = "Uniform(-10,10)", linetype = "Uniform(-10,10)"), show.legend = T) +
  geom_line(aes(x = seq(-10, 10, .01), y = dcauchy(seq(-10, 10,.01),0,(1/sqrt(2))), colour = "Cauchy(0,1/sqrt(2))", linetype = "Cauchy(0,1/sqrt(2))"), show.legend = T) +
  geom_line(aes(x = seq(-10, 10, .01), y = dnorm(seq(-10, 10,.01),0,1), colour = "Gauss(0,1)", linetype = "Gauss(0,1)"), show.legend = T) +
#  geom_line(aes(x = seq(-10, 10, .01), y = dcauchy(seq(-10, 10,.01),0,2), colour = "Cauchy(0,2)", linetype = "Cauchy(0,2)"), show.legend = T) +
  scale_colour_manual("",
                      breaks = c("Uniform(-10,10)","Cauchy(0,1/sqrt(2))", "Gauss(0,1)"),
                      values = c("black", "grey", "black")) +
  scale_linetype_manual("",
                      breaks = c("Uniform(-10,10)", "Cauchy(0,1/sqrt(2))", "Gauss(0,1)"),
                      values = c(1, 2, 3)) +
  ylab("Density") +
  xlab("Coefficient") +
  guides(color = guide_legend(nrow = 2)) +
  theme_bw() +
  common_ops 
  
prior_dist


