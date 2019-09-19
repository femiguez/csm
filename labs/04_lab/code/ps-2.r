library(lattice)

gdd <- seq(0,1700, 20)

pheno <- function(gdd, rstages = c(722, 833, 960, 1600)){

  stage.vec <- numeric(length(gdd))
  stage <- NA
  ## numbers between 0 and 99 are vegetative
  
  ## 1) Calculate GDD
  ## 2) When GDD exceed a threshold we add one more leaf
  ## stage <- 2
  ## 3) When GDD exceed rstages[1] then we are at R1
  ## stage <- 100
  ## 4) When at R2
  ## stage <- 200

  for(i in 1:length(gdd)){

    if(gdd[i] > rstages[1]) stage <- "R1"
    if(gdd[i] > rstages[2]) stage <- "R2"
    if(gdd[i] > rstages[3]) stage <- "R3"

    stage.vec[i] <- stage

  }

  res <- data.frame(gdd=gdd,stage=stage.vec)

  return(res)

}

stg <- pheno(gdd)

xyplot(stage ~ gdd, data = stg, type = 'l')
