logitppv <-
function(p, se, sp){log(se*p/((1-sp)*(1-p)))}

