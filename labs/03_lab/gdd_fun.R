## Function to calculate growing degree days

gdd_fun <- function(temp.max, temp.min,
                    dop = 90, doh = 300,
                    temp.base = 10){
  
  if(length(temp.max) != length(temp.min)) stop("The length of temp.max should be the same as the length of temp.min")
  
  temp.min <- pmax(temp.min, 10)
  temp.max <- pmin(temp.max, 30)
  
  temp.avg.m.base <- (temp.max + temp.min)/2 - temp.base
  
  temp.avg.m.base <- pmax(temp.avg.m.base, 0)
  
  gdd <- cumsum(temp.avg.m.base[dop:doh])
  
  ans <- data.frame(doy = dop:doh, gdd = gdd)
  
  ans
}

##ggplot(data = gdds, aes(x = doy, y = gdd)) + 
##  geom_line()