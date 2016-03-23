library(ggplot2)
library(dplyr)

load("net_data.Rdata")

individuals <- filter(individuals, Lake != "Tahoe")


p <- ggplot(individuals, aes(x=Length, fill=Group, color=Group)) +
  geom_density(alpha=0.4, kernel="gaussian", adjust=0.75) + 
  geom_rug(aes(y=0), position=position_jitter(width=0.2), sides="b") +
  facet_grid(trip ~ Lake, scales="free_y") + 
  theme_bw()
ggsave("graphics/zoop_length_distributions.pdf", p, width=8.5, height=11, units="in")
