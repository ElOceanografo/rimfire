library(ggplot2)
library(dplyr)
library(tidyr)

load("net_data.Rdata")

summary.counts <- counts %>%
  filter(!Group %in% c("Rotifers", "Nematode", "Hydroids")) %>%
  mutate(Genus = as.character(Genus),
         Group = as.character(Group)) %>%
  group_by(trip, Lake, Group, Genus) %>%
  summarize(Count = sum(Count)) %>%
  left_join(totals) %>%
  mutate(Percent = 100*Count / total)

summary.counts$Genus[is.na(summary.counts$Genus) & summary.counts$Group == "Copepods"] <- "Unk. Cope."
summary.counts$Genus[is.na(summary.counts$Genus) & summary.counts$Group == "Cladocerans"] <- "Unk. Clad."
summary.counts$Genus[is.na(summary.counts$Genus) & summary.counts$Group == "Ostracods"] <- "Unk. Ostracods"
genus.levels <- unique(arrange(summary.counts, desc(Group), desc(Genus))$Genus)

summary.counts <- summary.counts %>%
  mutate(Genus = factor(Genus, levels=genus.levels))


p <- summary.counts %>%
  filter(Lake %in% c("Independence", "Tahoe"), Percent > 0) %>%
  ggplot(aes(x=Genus, y=Percent, fill=Group)) + 
  geom_bar(stat="identity") + coord_flip() +
  scale_fill_grey(guide = guide_legend(reverse=TRUE)) +
  facet_wrap(~Lake) + 
  theme_minimal()
ggsave("graphics/zoop_composition_indy_tahoe.png", p, w=5, h=3, units="in")


p <- summary.counts %>%
  filter(Lake %in% c("Cherry", "Eleanor"), 
         Group != "Ostracods", Genus != "Diacyclops",
         Percent > 0) %>%
  ggplot(aes(x=Genus, y=Percent, fill=Group)) + 
    geom_bar(stat="identity") + coord_flip() +
    scale_fill_grey(guide = guide_legend(reverse=TRUE)) +
    facet_grid(Lake ~ trip) + 
    theme_minimal()
ggsave("graphics/zoop_composition_cherry_eleanor.png", p, w=7.5, h=4.5, units="in")

summary.cherry.eleanor <- counts %>%
  filter(Lake %in% c("Cherry", "Eleanor"), Group %in% c("Cladocerans", "Copepods")) %>%
  group_by(trip, Lake, Group) %>%
  summarise(Count = sum(Count)) %>%
  left_join(totals) %>%
  mutate(Percent = Count / total * 100)

ggplot(summary.cherry.eleanor, aes(x=trip, y=Percent, fill=Group)) + 
  geom_bar(stat="identity") + facet_wrap(~Lake)
summary.cherry.eleanor %>%
  select(-Count, -total) %>%
  spread(Group, Percent)

counts %>%
  filter(Lake %in% c("Cherry", "Eleanor")) %>%
  group_by(trip, Lake) %>%
  filter(Count > 0) %>%
  summarise(richness = length(unique(paste(Group, Genus, Species)))) %>%
  spread(Lake, richness)


## Individual length distributions
individuals <- individuals %>%
  filter(LifeStage!="Nauplius")

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
