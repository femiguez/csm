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

##png("../figs/lai-TT.png")
xyplot(lai.cum ~ AGDD, data = res,
       xlab = "Growing Degree Days",
       ylab = "Leaf Area Index")
##dev.off()

xyplot(Int.e ~ lai.cum, data = res,
       xlab = "LAI",
       ylab = "Interception")

## How does the extinction coefficient shange during the day?

hrs <- 8:16
res <- lightME(t.d = hrs)

chif <- function(cos.theta, chi.l=1){
    
    k0 <- sqrt(chi.l^2 + tan(acos(cos.theta))^2)
    k1 <- chi.l + 1.744 * (chi.l + 1.183)^-0.733
    k <- k0/k1

    k
}

kk <- chif(res$cos.th)

xyplot(kk ~ hrs, type = 'o')



del <- function(doy){

  dtr <- pi / 180
  ans <- -23.5 * cos((360 * ( doy + 10) /365) * dtr)

}

xx <- 1:365
yy <- del(xx)

pdf('../figs/sun-decl.pdf')
xyplot(yy ~ xx, type = 'l',
       xlab = 'day of the year',
       ylab = 'sun declination')
dev.off()
