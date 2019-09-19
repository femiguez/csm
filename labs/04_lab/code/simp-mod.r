## Script for lab 6: Data and graphics
## setwd("C:/Users/femiguez/Dropbox/Agron525_2017/ProblemSets/PS2/code")
## Loading packages or libraries
library(ggplot2)
### Read in weather data
list.files("../data")
wed <- read.csv("../data/ankeny-daily-2014-v3.csv")

qplot(x = day, y = rain, data = wed)

ggplot(data = wed, aes(x = day, y = maxt)) + 
  geom_point(aes(color = "max temp")) + 
  geom_point(aes(y = mint, color = "min temp")) + 
  ylab("Temperature (Celsius)") + 
  xlab("Day of the year")

## write a function to calculate gdds

gdd <- function(tmax, tmin, day1 = 120, dayn = 260){
  
  tbase <- 10 ## Base temperature  
  phy1 <- 46.7 ## Phyllochron
  
  ## day1 will be the emergence day of the crop
  ## dayn will be the day of physiological maturity
  
  n <- length(tmax); n1 <- length(tmin)
  
  j <- 1
  gdd_cum <- 0
  gdd_today_vec <- numeric(c(dayn - (day1-1)))
  gdd_cum_vec <- numeric(c(dayn - (day1-1)))
  
  for(i in 1:n){
    
    tmax.b <- min(c(30, tmax[i]))
    tmin.b <- max(c(10, tmin[i]))
    
    gdd_today <- (tmax.b + tmin.b)/2 - tbase
    
    gdd_today <- max(c(0, gdd_today))
    
    if(i >= day1 && i <= dayn){
      gdd_cum <- gdd_cum + gdd_today
      gdd_today_vec[j] <- gdd_today
      gdd_cum_vec[j] <- gdd_cum
      
      ## Let's count the number of leaves
      nleaves <- gdd_cum %% phy1
      
      j <- j + 1
    }
  }
##  print(j)
  
  data.frame(gdd = gdd_today_vec, gdd.cum = gdd_cum_vec)
}

gddsc <- gdd(wed$maxt, wed$mint)

qplot(x = 120:260, y = gddsc$gdd.cum, ylab = "Cum GDD" , xlab = "DOY")

scm <- function(rad, tmax, tmin, k = 0.6, rue = 1.3, laic = 0.0102,
                day1 = 120, dayn = 260, maxlai = 6){

    ## Inputs:
    ## rad = radiation
    ## tmax = maximum temperature
    ## tmin = minimum temperature
    ## k = extinction coefficient
    ## rue = radiation use efficiency
    
    ## We need different components for our model
    ## First we will calculate LAI simply as a function of GDD
    ## Second we will calculate interception as a funciton of LAI and the extinction coefficient
    ## Third we will calculate biomass as a function of radiation, interception and radiation use efficiency

    gddc <- gdd(tmax, tmin)$gdd.cum

    laic <- laic * gddc
    laic[laic > maxlai] <- maxlai

    int <- (1 - exp(-k * laic))

    bio <- rad[day1:dayn] * int * rue

    ans <- data.frame(doy = day1:dayn, gddc = gddc, laic = laic, int = int, bio = bio)

    ans

    }
    

ans <- scm(wed$radn, wed$maxt, wed$mint)

ggplot(data = ans, aes(x = gddc, y = bio)) +
    geom_point()

ggplot(data = ans, aes(x = gddc, y = cumsum(bio))) +
    geom_point()
