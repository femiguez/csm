## Lab one
## Example of importing data into R
library(readxl)
library(ggplot2)
library(dplyr)

## Which files are available?
list.files()

fldm <- read_excel("BRKAbsub_flowering.xlsx", sheet = 3)

dim(fldm)

head(fldm)

summary(fldm)

fldm$Genotype <- factor(fldm$Genotype)

sapply(fldm, class)

ggplot(data = fldm, aes(x = `N rate`, y = MP)) + 
  geom_point()

ggplot(data = fldm, aes(x = `N rate`, y = MP, color = Genotype)) + 
  geom_point() 

ggplot(data = fldm, aes(x = `N rate`, y = MP, color = Genotype)) + 
  geom_smooth(method = "loess")

ggplot(data = fldm, aes(x = `N rate`, y = MP)) + 
  facet_wrap(~ Genotype) + 
  geom_point() + geom_jitter()

ggplot(data = fldm, aes(x = Genotype, y = MS, color = `N rate`)) + 
  geom_point()

ggplot(data = fldm, aes(x = Genotype, y = MS)) + 
  geom_violin()

ggplot(data = fldm, aes(x = Genotype, y = MS)) + 
  geom_violin() + geom_point()

ggplot(data = fldm, aes(x = Genotype, y = MS)) + 
  geom_violin() + geom_point() + geom_jitter()

ggplot(data = fldm, aes(x = Genotype, y = MS, color = as.factor(`N rate`))) + 
  geom_violin() + geom_point() + geom_jitter()

ggplot(data = fldm, aes(x = Genotype, y = MS, color = as.factor(`N rate`))) + 
  geom_point() + geom_jitter()

ggplot(data = fldm, aes(x = MS, y = MP)) + 
  geom_point() + xlab("Silking") + ylab("Pollen shed") + 
  geom_smooth(method = "lm")

## Data manipulation
filter(fldm, MS > 45)
filter(fldm, MS > 44)

filter(fldm, Genotype == "LH195/Mo17" & `N rate` == 1)

fldm %>% filter(Genotype == "LH195/Mo17" & `N rate` == 1)

fldm %>% group_by(Genotype) %>% summarise(mean.MP = mean(MP))

fldm %>% group_by(Genotype) %>% summarise(mean.MP = mean(MP), mean.MS = mean(MS))

fldm %>% 
  group_by(Genotype) %>% 
  summarise(mean.MP = mean(MP), 
            mean.MS = mean(MS), 
            ASI = mean.MS - mean.MP,
            sd.ASI = sd(MS - MP))
