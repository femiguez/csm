## Simple method for modeling LAI based on the RUEmod
library(BioCro)

data(cmiWet)
names(cmiWet) <- c("year","day","solar","avgTemp")
## Solar MJ/m2
## Temp in F
cmi05 <- cmiWet[cmiWet$year == 2005,]

args(RUEmod)

res <- RUEmod(cmi05$Rad, cmi05$avgTemp)

names(res)

png("../figs/lai-TT.png")
xyplot(lai.cum ~ AGDD, data = res,
       xlab = "Growing Degree Days",
       ylab = "Leaf Area Index")
dev.off()

## For corn we have a model that is based on individual leaves

  data(weather04)
  res <- MaizeGro(weather04, plant.day = 110, emerge.day = 120, harvest.day=300,
  laiControl = laiParms(lai.method = "ind-leaf-Lizaso"))

png("../figs/LAI-MaizeGro.png")
  plot(res, plot.kind = "LAI")
dev.off()
