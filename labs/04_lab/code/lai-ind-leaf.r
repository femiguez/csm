## This script has the purpose of implementing the modeling of
## individual leaves of corn based on Lizaso

## First steps In the paper by Lizaso the key is Eq 3.  This describes
## the rate of change, but to be able to run this equation we first need to esimate Aei

## Some important inputs

LT <- 20 ## Total number of leaves
Aex <- 800 ## Size of the largest leaf (cm^2)
A1 <- -5.61 ## Empirical coefficient which describes the shape of the leaf size curve (Table 1)
A2 <- -0.59 ## Empirical coefficient which describes the shape of the leaf size curve (Table 1)
LLx <- 700 ## Longevity of the largest leaf

## Equation for Aei

Aei <- function(LNi = 10, LT = 20, A1 = -5.61, A2 = -0.59, Aex = 800){

    ## In this function the main parameter is LNi or current leaf number
    
    ## The equation is
    ## Aei = Aex * exp(A1 * ((LNi - LNx)/(LNx - 1))^2 + A2 * ((LNi - LNx)/(LNx - 1))^3)

    LNx <- 0.67 * LT

    ## Part 0
    p0 <- ((LNi - LNx)/(LNx - 1))
    ## Part 2
    p1 <- A1 * p0^2
    ## Part 3
    p2 <- A2 * p0^3

    aei <- Aex * exp(p1 + p2)

    aei ## This is the maximum size of the current leaf

}

nl <- 1:20
saei <- Aei(nl)

## Using base plot
plot(saei ~ nl, type = "o", xlab = "Leaf number", ylab = "Final size of leaf (cm^2)")

## Using lattice
xyplot(saei ~ nl, type = "o", xlab = "Leaf number", ylab = "Final size of leaf (cm^2)")

## Using ggplot2
qplot(x = nl, y = saei, geom = c("point","line"), xlab = "Leaf number", ylab = "Final size of leaf (cm^2)")

    
