varlppv <-
function(x0, x1, k, p){
n1<-sum(x1)
n0<-sum(x0)
seest<-setil(x1=x1, k=k)
spest<-sptil(x0=x0, k=k)
vartil <- ((1-seest[1])/seest[1])*(1/(n1+k^(2))) + (spest[1]/(1-spest[1]))*(1/(n0+k^(2)))
varhat <- ((1-seest[2])/seest[2])*(1/n1) + (spest[2]/(1-spest[2]))*(1/n0)
return(c(vartil=vartil, varhat=varhat))
}

