setil <-
function(x1, k){
n1 <- sum(x1)
sehat <- x1[1]/n1
setil <- ( n1*sehat + k^(2)/2 )/(n1+k^(2))
return(c(setil=setil, sehat=sehat))
}

