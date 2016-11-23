library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

load("single_targets.Rdata")
sound.speed <- 1500
a <- mean(c(-4.867, -4.898, -5.186))
b <- mean(c(2.98, 2.99, 3.103))

targets <- filter(targets, TS_uncomp > -60, TS_comp < -30, Target_range < 100)
targets <- mutate(targets, 
                  # Foote 1987, from Simmonds and MacLennan (in cm)
                  length = 10^((TS_uncomp + 67.4) / 20),
                  # convert length to mm for W-L equations
                  weight = 10^(a + b * log10(length * 10)))


ggplot(targets, aes(x=TS_comp, y=..density..)) +
  geom_histogram() +
  facet_grid(Lake ~ trip) + coord_flip()

ggplot(targets, aes(x=length, y=..density..)) +
  geom_histogram() +
  facet_grid(Lake ~ trip) + coord_flip()

ggplot(targets, aes(x=weight, y=..density..)) +
  geom_histogram() +
  facet_grid(Lake ~ trip) + coord_flip()

mean.ts.fish <- targets %>%
  group_by(Lake, trip) %>%
  summarise(sigma = mean(10^(TS_comp/10)),
            TS = 10*log10(sigma),
            length = mean(length),
            weight = mean(weight),
            n = n())

reshape2::dcast(mean.ts.fish, Lake ~ trip, value.var = "TS")
reshape2::dcast(mean.ts.fish, Lake ~ trip, value.var = "length")
reshape2::dcast(mean.ts.fish, Lake ~ trip, value.var = "weight")
reshape2::dcast(mean.ts.fish, Lake ~ trip, value.var = "n")

mean.ts.fish <- arrange(mean.ts.fish, trip, Lake)
write.csv(mean.ts.fish, file = "fish_TS.csv", row.names = F)

ts.display.table.fish <- targets %>%
  filter(Lake != "Independence") %>%
  group_by(trip, Lake) %>%
  summarise(TS.mean = 10*log10(mean(10^(TS_comp/10))),
            TS.sd = sd(TS_comp),
            length.mean = mean(length),
            length.sd = sd(length),
            weight.mean = mean(weight),
            weight.sd = sd(weight),
            n = n())
ts.display.table.fish[, 3:8] <- signif(ts.display.table.fish[ , 3:8], 2)
for (col in c("TS.sd", "length.sd", "weight.sd")) {
  ts.display.table.fish[[col]] <- paste0("(", ts.display.table.fish[[col]], ")")
}
write.csv(ts.display.table.fish, "nets/ts.display.table.fish.csv")
