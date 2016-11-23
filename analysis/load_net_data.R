library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)

net.meta <- read.csv("nets/RimFireZooplanktonSampleList.csv", stringsAsFactors = F) %>%
  filter(SplitNumber %in% c(0, 2) | Year == 2013) %>%
  mutate(trip = paste0(Year, "-", str_pad(Month, 2, pad="0")),
         SplitNumber = as.integer(as.character(SplitNumber))) %>%
  select(trip, Lake, MaxDepth, NetType, SplitNumber, VolFiltered, 
         Biovolume, TERCSplitFactor, Dilution=TotalDilutionFactor)

individuals <- read.csv("nets/RimFireZooplanktonMeasurements.csv", stringsAsFactors = F)
individuals$Month <- plyr::revalue(individuals$Month, c(Sept="09", Oct="10", April="04", June="06"))
individuals <- mutate(individuals, 
                      Group = gsub("Cladocera", "Cladocerans", Group),
                      Group = gsub("Penilia", "Cladocerans", Group))


counts <- read.csv("nets/RimFireZooplanktonCounts.csv", skip=5, stringsAsFactors = F) %>%
  mutate(SplitNumber = gsub("S", "", SplitNumber)) %>%
  melt(id.vars=1:8, variable.name="taxon.code", value.name="Count") %>%
  mutate(taxon.code = as.character(taxon.code)) %>%
  replace_na(list(Count = 0))

counts$taxon.code[counts$taxon.code == "X"] <- "X.0"

taxa <- read.csv("nets/RimFireZooplanktonCounts.csv", nrows=5, header=F) %>%
  t() %>% as.data.frame(stringsAsFactors=F) %>%
  slice(9:n()) %>%
  rename(Group = V1, Order = V2, Genus = V3, Species = V4, LifeStage = V5) %>%
  mutate(Group = gsub("Penilia", "Cladocera", Group),
         taxon.code = paste0("X.", 0:(n() - 1))) %>%
  droplevels()

counts <- left_join(counts, taxa, by="taxon.code")
individuals <- left_join(individuals, taxa,
                         by=c("Group", "Order", "Genus", "Species", "LifeStage"))

counts <- mutate(counts, trip = paste0(Year, "-", str_pad(Month, 2, pad="0")))
individuals <- mutate(individuals, trip = paste0(Year, "-", str_pad(Month, 2, pad="0")))



individuals <- individuals %>%
  filter(!((trip == "2014-09") & (Lake=="Tahoe")),
         ! (Lake == "Eleanor" & trip == "2014-09" & SplitNumber == 1)) 
# This sample (Eleanor 2014-09-30) appears to be a mislabeled duplicate

counts <- counts %>%
  filter(!((trip == "2014-09") & (Lake=="Tahoe")),
         ! (Lake == "Eleanor" & trip == "2014-09" & SplitNumber == 1)) %>%
  mutate(Genus = gsub("Diaptomous", "Leptodiaptomus", Genus))

save(counts, individuals, taxa, net.meta, file = "net_data.Rdata")
