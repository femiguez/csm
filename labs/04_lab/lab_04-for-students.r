## Lab 4: build your own crop model
##
## Step 1:
## Spend 10 minutes thinking about how you will write your code
## Paper and pencil
##
## Step 2: 
## Execute!

cmi05 <- read.csv("cmi2005.csv")

## I will loop through each row of the data.frame
## Step 0: convert from F to C (temperature)
## Step 1: Calculate GDDs
## Step 2: Build a linear relationship between GDDs and LAI
## Step 3: Calculate interception based on LAI
## Step 4: Determine a value of Radiation Use Efficiency for Mxg
## Step 5: Use equation Y = Q x I x e x H to calculate biomass

doe <- 115 ## day of emergence
doh <- 280 ## last day the crop is 'active' (day of harvest)
lai.cum <- numeric(365)
biomass.curr <- numeric(365)
biomass.cum <- numeric(365)
biomass.old <- 0
gdd.cum <- numeric(365)
int.rad.cum <- numeric(365)
old.gdd <- 0
## Need to get the cummulative
## Extinction coefficient
k <- 0.6
## Need to come up with a value of RUE for 
## Miscanthus
## How about 2? 2 g / MJ for PAR
rue <- 2.4
harvest.index <- 0.7

for(i in 1:nrow(cmi05)){
  
  ## i is an index, but it coincides with the 
  ## day of the year

  if(i < doe) next
  if(i > doh) break
  ## Next is a command that will jump to the next iteration
  ## Convert to Celsius
  tempC <- (cmi05$avgTemp[i] - 32) * 5/9
  ## Calculate growing degree days
  gdd <- (tempC - 10) + old.gdd
  if(gdd < 0) gdd <- 0
  old.gdd <- gdd
  gdd.cum[i] <- gdd
  ## Great! Now I can calculate LAI with my magic formula
  lai <- (gdd * 0.0102) 
  if(lai > 7) lai <- 7
  lai.cum[i] <- lai
  ## Yeah! I calculated LAI
  ## Let's do interception
  int.rad <- (1 - exp(-k * lai))
  int.rad.cum[i] <- int.rad
  ## That easy?
  ## Solar radiation
  par <- cmi05$solar[i] * 0.45
  biomass.today <- par * int.rad * rue * harvest.index
  biomass <-  par * int.rad * rue * harvest.index + biomass.old
  biomass.old <- biomass
  biomass.cum[i] <- biomass
  biomass.curr[i] <- biomass.today
}

plot(gdd.cum, lai.cum, xlim = c(0,500), main = "GDDs and LAI")

plot(lai.cum, int.rad, main = "LAI and Int Rad")

plot(gdd.cum, biomass.cum, main = "GDDs and Biomass",
     ylab = "Biomass (g / m^2)",
     xlab = "Growing Degree Days (base 10)")

plot(gdd.cum, biomass.curr, main = "GDDs and Biomass Today",
     ylab = "Daily Biomass (g / m^2)",
     xlab = "Growing Degree Days (base 10)")
