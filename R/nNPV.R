nNPV <-
function(propP, se, sp, prev, NPV0, conf.level=0.95, power=0.8)
{
propQ<-(1-propP)
# Eqn 2.9
sigmasq1 <- (1-se)/(se*propP)+(sp)/((1-sp)*propQ)
# Eqn 2.8
sigmasq2 <- (se)/((1-se)*propP)+(1-sp)/((sp)*propQ)
# Eqn 2.5
phi1 <- log(1-sp)-log(se)
# Eqn. 2.6
phi2 <- log(1-se)-log(sp)

TRUEnpv<-npv(p=prev, se=se, sp=sp)
z1alpha <- qnorm(p=conf.level)
z1beta <- qnorm(p=power)
if(TRUEnpv<=NPV0){n<-rep(NA)}else{
  nest <- ((z1alpha+z1beta)^2 * sigmasq2) / (phi2-log(((1-prev)/prev)*((1-NPV0)/NPV0)))^2
  n<-ceiling(nest)
 }

out<-list(n=n, se=se, sp=sp, prev=prev, NPV0=NPV0, TRUENPV=TRUEnpv, propP=propP, power=power, conf.level=conf.level)
return(out)
}

