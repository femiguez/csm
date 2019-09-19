library(ggplot2)
source("cornPheno.R")
## Read in the data
ames.temp <- read.table("IA0200.txt", header = TRUE, sep = ",")

ames.temp$date <- as.Date(ames.temp$day)
ames.temp$year <- format(ames.temp$date,"%Y")

ames.temp.2014 <- subset(ames.temp, year == 2014)

cornv <- cornPheno(ames.temp.2014$highc, ames.temp.2014$lowc)

ggplot(data = cornv, aes(x = gdd, y = pheno)) + 
  geom_line()

doy1 <- 120
doyn <- 260

corn.pheno.all <- data.frame(year = rep(unique(ames.temp$year), each = doyn),
                             doy = NA, gdd = NA, v.pheno = NA, r.pheno = NA)

for(i in 1:length(unique(ames.temp$year))){
  
  yr <- unique(ames.temp$year)[i]
  
  ames.tmp <- subset(ames.temp, year == yr)
  
  corn.ph.tmp <- cornPheno(ames.tmp$highc, ames.tmp$lowc, doh = doyn)
  
  ii <- (i - 1)*doyn + 1
  ie <- i * doyn
  corn.pheno.all[ii:ie,2:5] <- corn.ph.tmp
}

corn.pheno.veg <- subset(corn.pheno.all, doy > 119 & doy < 220)

ggplot(data = corn.pheno.veg, aes(x = doy, y = v.pheno, color = year)) +
  geom_step()

corn.pheno.rep <- subset(corn.pheno.all, doy > 175)

ggplot(data = corn.pheno.rep, aes(x = doy, y = r.pheno, color = year)) +
  geom_step()

## I have a data frame with temperatures
## I could loop over every row and perform calculations
## I will do it for one year first

n.days.2014 <- dim(ames.temp.2014)[1]
doe <- 120

for(i in 1:n.days.2014){
  
  if(i < doe) next
  
  
}

