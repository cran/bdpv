sptil <-
function(x0, k){
n0 <- sum(x0)
sphat <- x0[2]/n0
sptil <- ( n0*sphat + k^(2)/2 )/(n0+k^(2))
return(c(sptil=sptil, sphat=sphat))
}

