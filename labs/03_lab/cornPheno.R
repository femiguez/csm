## function to simulate corn phenology

cornPheno <- function(temp.max, temp.min, temp.base = 10, 
                      phyll1 = 46.7, phyll2 = 31.1, 
                      dop=120, doh=250, max.leaves = 20,
                      method = c("gdd","chu")){

  if(length(temp.max) > 366) stop('length of temp.max should be less than 366')

  if(length(temp.min) > 366) stop('length of temp.min should be less than 366')

  gdd <- numeric(doh)
  chu <- numeric(doh)

  old.gdd <- 0
  old.chu <- 0

  v.pheno <- numeric(doh)
  r.pheno <- numeric(doh)

  veg.stage <- 0
  
  rep.stage <- 0
  
  for(i in 1:doh){

    if(i < dop){
      
      gdd[i] <- 0

      v.pheno[i] <- 0
      
    }else{

      if(method == "gdd"){
        Tmax <- min(c(temp.max[i], 30))
        Tmin <- max(c(temp.min[i], 10))
        gdd[i] <- old.gdd + ((Tmax + Tmin)/2 - temp.base)
      }else{
        chu.min <- 1.8 * (temp.min[i] - 4.4)
        chu.max <- 3.33 * (temp.max[i] - temp.base) - 0.084 * (temp.max[i] - temp.base)^2
      }

      if(veg.stage < 10){
        veg.stage <- gdd[i] %/% phyll1
        gddV10 <- gdd[i]
      }else{
        veg.stage <- (gdd[i] - gddV10) %/% phyll2 + 10
      }

      veg.stage <- min(c(veg.stage,max.leaves))
      v.pheno[i] <- veg.stage
      
      if(gdd[i] > 722) rep.stage <- 1
      if(gdd[i] > (722 + 111)) rep.stage <- 2
      if(gdd[i] > (722 + 222)) rep.stage <- 3
      if(gdd[i] > (722 + 322)) rep.stage <- 4
      if(gdd[i] > (722 + 361)) rep.stage <- 5
      if(gdd[i] > (722 + 667)) rep.stage <- 6

      r.pheno[i] <- rep.stage
      
      old.gdd <- gdd[i]

    }
      
  }

    res <- data.frame(doy = 1:doh, gdd = gdd, 
                v.pheno = v.pheno, r.pheno = r.pheno)
    
}

##pheno <- factor(c("VE","V1","V2","R1","R2"), 
##                levels=c("VE","V1","V2","R1","R2"))

##xyplot(pheno ~ 1:5)    

  
