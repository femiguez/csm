#LAI function
LAI <- function(J, t, abv, blw, L=42, x=-93, x0=-90){
  ## Author Joseph Iverson
  ## Equations and theory are based on the documentation of the LI-80 Accupar ceptopmeter
  ## J=day of year
  ## t=time in hours 0-24
  ## L=latitude
  ## x=longitude
  ## x0=standard meridian
	t <- t-1	#DLS
	D <- asin(0.39785*sin(4.869+0.0172*J+0.03345*sin(6.224+0.0172*J)))  #declination
	LC <- (x-x0)/15		#longitude correction
	p <- (279.575+0.986*J)*pi/180		#phi
	ET <-	((-104.7*sin(p)+596.2*sin(2*p)+4.3*sin(3*p)-12.7*sin(4*p)-429.3*cos(p)-2*cos(2*p)+19.3*cos(3*p))/3600)	#equation of time
	t0 <- 12-LC-ET
	theta <- acos(sin(L*pi/180)*sin(D)+cos(L*pi/180)*cos(D)*cos((t-t0)*pi/12))	#zenith angle
	K <- 1/(2*cos(theta))	#extinction coefficient
	tau <- blw/abv
	r <- abv/(cos(theta)*2550)
	if (r > .82){r <- .82}
	if (r < .2){r <- .2}
	fb <- 1.395+r*(-14.43+r*(48.57+r*(-59.024+r*24.835)))
	L <- ((1-1/(2*K))*fb-1)*log(tau)/((0.86)*(1-0.47*fb))
	return(L)

}

## Testing the function

Js <- 120
t <- 12
abv <- 1800
blw <- 200

LAI(Js, t, abv, blw)
