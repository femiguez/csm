## How does the extinction coefficient change during the day?



hrs <- 8:16
res <- lightME(t.d = hrs)

chif <- function(cos.theta, chi.l=1){
    
    k0 <- sqrt(chi.l^2 + tan(acos(cos.theta))^2)
    k1 <- chi.l + 1.744 * (chi.l + 1.183)^-0.733
    k <- k0/k1

    k
}

kk <- chif(res$cos.th)

xyplot(kk ~ hrs)

plot(acos(res$cos.th) * (180/pi))
