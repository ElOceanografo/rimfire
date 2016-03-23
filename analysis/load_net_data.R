library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)

net.meta <- read.csv("nets/RimFireZooplanktonSampleList.csv") %>%
  filter(SplitNumber %in% c(0, 2)) %>%
  select(Year, Month, Day, Lake, MaxDepth, NetType, SplitNumber, VolFiltered, 
         Biovolume, Dilution=TotalDilutionFactor) %>%
  mutate(SplitNumber = as.integer(as.character(SplitNumber)))

individuals <- read.csv("nets/RimFireZooplanktonMeasurements.csv")
levels(individuals$Month) <- c(4, 6, 10, 9)
individuals <- mutate(individuals, 
                      Month = as.integer(as.character(Month)),
                      Group = gsub("Cladocera", "Cladocerans", Group),
                      Group = gsub("Penilia", "Cladocerans", Group))


counts <- read.csv("nets/RimFireZooplanktonCounts.csv", skip=5) %>%
  mutate(SplitNumber = gsub("S", "", SplitNumber)) %>%
  melt(id.vars=1:8, variable.name="taxon.code", value.name="Count") %>%
  mutate(taxon.code = as.character(taxon.code)) %>%
  replace_na(list(Count = 0))

counts$taxon.code[counts$taxon.code == "X"] <- "X.0"

taxa <- read.csv("nets/RimFireZooplanktonCounts.csv", nrows=5, header=F) %>%
  t() %>% as.data.frame() %>%
  slice(9:n()) %>%
  rename(Group = V1, Order = V2, Genus = V3, Species = V4, LifeStage = V5) %>%
  mutate(Group = gsub("Penilia", "Cladocera", Group),
         taxon.code = paste0("X.", 0:17)) %>%
  droplevels()

counts <- left_join(counts, taxa, by="taxon.code")
individuals <- left_join(individuals, taxa,
                         by=c("Group", "Order", "Genus", "Species", "LifeStage"))

counts <- mutate(counts, trip = paste0(Year, "-", str_pad(Month, 2, pad="0")))
individuals <- mutate(individuals, trip = paste0(Year, "-", str_pad(Month, 2, pad="0")))

save(counts, individuals, taxa, file = "net_data.Rdata")
