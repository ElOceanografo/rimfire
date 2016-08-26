library(ggplot2)
library(dplyr)

load("net_data.Rdata")

individuals <- individuals %>%
  filter(!((trip == "2014-09") & (Lake=="Tahoe")))

counts <- counts %>%
  filter(!((trip == "2014-09") & (Lake=="Tahoe")))


totals <- counts %>%
  group_by(trip, Lake) %>%
  summarize(total = sum(Count))

summary.counts <- counts %>%
  filter(Group %in% c("Copepods", "Cladocerans")) %>%
  mutate(Genus = as.character(Genus),
         Group = as.character(Group)) %>%
  group_by(trip, Lake, Group, Genus) %>%
  summarize(Count = sum(Count)) %>%
  left_join(totals) %>%
  mutate(Percent = 100*Count / total)

summary.counts$Genus[is.na(summary.counts$Genus) & summary.counts$Group == "Copepods"] <- "Unk. Cope."
summary.counts$Genus[is.na(summary.counts$Genus) & summary.counts$Group == "Cladocerans"] <- "Unk. Clad."
genus.levels <- unique(arrange(summary.counts, desc(Group), desc(Genus))$Genus)

summary.counts <- summary.counts %>%
  mutate(Genus = factor(Genus, levels=genus.levels))


p <- summary.counts %>%
  filter(Lake %in% c("Independence", "Tahoe")) %>%
  ggplot(aes(x=Genus, y=Percent, fill=Group)) + 
  geom_bar(stat="identity") + coord_flip() +
  scale_fill_grey(guide = guide_legend(reverse=TRUE)) +
  facet_wrap(~Lake) + 
  theme_minimal()
ggsave("graphics/zoop_composition_indy_tahoe.png", p, w=5, h=3, units="in")


p <- summary.counts %>%
  filter(Lake %in% c("Cherry", "Eleanor")) %>%
  ggplot(aes(x=Genus, y=Percent, fill=Group)) + 
    geom_bar(stat="identity") + coord_flip() +
    scale_fill_grey(guide = guide_legend(reverse=TRUE)) +
    facet_grid(Lake ~ trip) + 
    theme_minimal()
ggsave("graphics/zoop_composition_cherry_eleanor.png", p, w=7.5, h=4.5, units="in")




individuals <- individuals %>%
  filter(LifeStage=="Adult")

p.indy.tahoe <- individuals %>%
  filter(Lake %in% c("Independence", "Tahoe")) %>%
  ggplot(aes(x=Group, y=Length)) +
  geom_boxplot() + 
  facet_wrap(~Lake) + 
  theme_bw()

ggsave("graphics/zoop_length_indy_tahoe.png", p.indy.tahoe, 
       width=6, height=3, units="in")

p.cherry.eleanor <- individuals %>%
  filter(Lake %in% c("Cherry", "Eleanor")) %>%
  ggplot(aes(x=Group, y=Length)) +
    geom_boxplot() + 
    facet_grid(Lake ~ trip) + 
    theme_bw()
  
ggsave("graphics/zoop_length_cherry_eleanor.png", p.cherry.eleanor,
       width=8, height=4, units="in")
