## Load libraries
library(ggplot2)

## Preliminary
## getwd()
## setwd("~")
## How to clean up objects
rm(list=ls())
## Temp in F
x1 <- 40
x2 <- 50
x3 <- 70
x4 <- 80
## Convert to Celsius
x1.C <- (x1 - 32) * 5/9
x2.C <- (x2 - 32) * 5/9
x3.C <- (x3 - 32) * 5/9
x4.C <- (x4 - 32) * 5/9

print(c(x1.C,x2.C,x3.C,x4.C))

## Control Flow
## if statement
x <- 50

units <- "F"

if(units == "F"){
  x <- (x - 32) * 5/9
}else{
  print("what should I do?")
}

sum_x <- 0
for(i in 1:10){
  sum_x <- sum_x + i
  cat("the current loop element is",i,"\n")
  cat("the current cumulative sum is",sum_x,"\n")
}

x <- 100

F <- c(1,1)
n <- length(F)

while(F[n] <= 100){
  cat("n = ",n," F[n] =",F[n],"\n")
  n <- n + 1
  F[n] <- F[n - 1] + F[n-2]
}

add2values <- function(x,y){
  
  result <- x + y
  
  return(result)
}

## Working on problem set 2
## What do I need?
## I promise I will write pseudo-code, but I can't wait to read in my data
## Where is the datafile?
## Is it in my current directory? YES!
ames.temp <- read.table("IA0200.txt")
## Yikes! That didn't work! It is separated by commas.
ames.temp <- read.table("IA0200.txt", sep = ",")
## How do I get R to read the column names?!
ames.temp <- read.table("IA0200.txt", sep = ",", header = TRUE)
## Success! I was able to read in the data.
## What do we have here?
## Temperature in F and C
## Some pretty graphs
ames.temp$date <- as.Date(ames.temp$day)
ames.temp$year <- format(ames.temp$date,"%Y")
head(ames.temp)

ggplot(data = ames.temp, aes(x = doy, y = high)) + 
  geom_point() + 
  facet_wrap(~ year)