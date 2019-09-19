## Calculating the sun declination

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

## Function to calculate the cosine of the zenith angle

coszang <- function(lat, doy, t.d, t.sn = 12){

  dtr <- pi/180
  
  omega <- lat * dtr

  deltR <- del(doy) * dtr

  t.f <- (15 * (t.d - t.sn)) * dtr

  ss <- sin(deltR) * sin(omega)
  cc <- cos(deltR) * cos(omega)

  coszenithangle <- ss + cc * cos(t.f)
  coszenithangle <- ifelse(coszenithangle < 1e-10, 1e-10, coszenithangle) 
  
  coszenithangle
}


lat <- 42
dtr <- pi / 180

k <- 1

doyhr.angle <- expand.grid(hour = 0:23, doy = 1:365, cos.th = NA)

for(i in 1:365){

  for(j in 0:23){

   doyhr.angle[k,3] <- coszang(lat = lat, doy = i, t.d = j)

   k <- k + 1
 }
}

xyplot(cos.th ~ hour | factor(doy) , subset = doy %in% c(120: 130),
       data = doyhr.angle, type = 'o')


## We can also calculate daylength, sunup and sundown

daylen <- function(lat, doy){

  dtr <- pi/180

  omega <- lat * dtr

  deltR <- del(doy) * dtr
  
  coshour <- -tan(omega) * tan(deltR)
  coshourdeg <- (1/dtr) * coshour
  daylength <- 2 * (1/dtr) * acos(coshour) / 15
  sunup <- 12 - daylength / 2
  sundown <- 12 + daylength / 2

  list(daylength = daylength, sunup = sunup, sundown = sundown)

}

## Testing the daylength function

xx <- 1:365
yy <- daylen(lat = 42, xx)

xyplot(yy$daylength + yy$sunup + yy$sundown ~ xx,
       type = 'l',
       lty = 1:3,
       lwd = 2,
       col = c('blue', 'red', 'purple'),
       xlab = 'Day of the year',
       ylab = 'Hours',
       key=list(text = list(c('day length', 'sun up', 'sun down')),
         lty = 1:3, col = c('blue', 'red', 'purple'),
         lines = TRUE, lwd = 2))

## Proportion of direct and diffuse radiation

light <- function(irr=NULL, lat, doy, t.d, atm.P = 1e5){

  dtr <- pi / 180

  PP.o <- 1e5/atm.P
  Solar_Constant <- 2650
  if(missing(irr)) irr <- Solar_Constant
  alpha <- 0.85

  coszenithangle <- coszang(lat = lat, doy = doy, t.d = t.d)
  
  I.dir <- irr * (alpha^((PP.o)/coszenithangle)) * coszenithangle

  I.diff <- 0.5 * irr * (1 - alpha ^ ((PP.o)/coszenithangle)) * coszenithangle

  list(I.dir = I.dir, I.diff = I.diff)

}


doyhr.light <- expand.grid(hour = 0:23, I.dir = NA, I.diff = NA)

for(j in 0:23){
  tmp <- light(irr = 2000, lat = lat, doy = 190, t.d = j)
  doyhr.light[j + 1,2] <- tmp$I.dir
  doyhr.light[j + 1,3] <- tmp$I.diff
}

## the previous function is vectorized, even if you don't notice
## So there is no need for a loop

ans <- light(irr = 2000, lat = lat, doy = 190, t.d = 0:23)

pdf('../figs/Dir-Diff-Int.pdf')
xyplot(I.dir + I.diff ~ 0:23 , 
       data = ans, type = 'l')
dev.off()


## Illustrating the extinction coefficient

extcoef <- function(cza, chi) {

  ## cza is cosine of the zenith angle
  num <- sqrt(chi^2 + tan(acos(cza))^2) * cza
  den <- chi + 1.774*(chi + 1.182)^-0.733

  ans <- num/den

  ans

}

cza <- coszang(lat = 40, doy = 120 , t.d = 0:23)

yy <- extcoef(cza, 1.43)

plot(0:23, yy)
