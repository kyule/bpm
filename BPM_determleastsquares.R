BPM_determresids <- function(states,parms,inits,times){
  states <- c(M,B,P,Mp,Mb)
  parms <- c(k,aBM,aPM,aBP,cMB,cMP,cPB,r,m,deP,deB,deM,deMp,deMb,h)
  inits <- c(B.init,P.init,M.init,Mp.init,Mb.init)
  times <- seq(1,10000,h)
  
  outs <- BPMdeterm(c(k,aBM,aPM,aBP,cMB,cMP,cPB,r,m,deP,deB,deM,deMp,deMb,h),c(B.init,P.init,M.init,Mp.init,Mb.init),seq(1,10000,h))
  Mdet <- outs[,1]
  Bdet <- outs[,2]
  Pdet <- outs[,3]
  Mpdet <- outs[,4]
  Mbdet <- outs[,5]
  
  resids <- sum((B-Bdet)^2+(TheD-Ddet)^2+)
  
}