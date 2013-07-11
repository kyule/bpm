BPM_determresids <- function(parms,M,B,P,Mp,Mb,r,k,m,h,M.init,B.init,P.init,Mp.init,Mb.init,times){
  aBM <- parms[1]
  aPM <- parms[2]
  aBP <- parms[3]
  cMB <- parms[4]
  cMP <- parms[5]
  cPB <- parms[6]
  deP <- parms[7]
  deB <- parms[8]
  deM <- parms[9]
  deMp <- parms[10]
  deMb <- parms[11]
  
  outs <- BPMdeterm(c(aBM,aPM,aBP,cMB,cMP,cPB,deP,deB,deM,deMp,deMb),k,r,m,h,B.init,P.init,M.init,Mp.init,Mb.init,times)
  Mdet <- outs[,1]
  Bdet <- outs[,2]
  Pdet <- outs[,3]
  Mpdet <- outs[,4]
  Mbdet <- outs[,5]
  
  resids <- sum((B-Bdet)^2+(M-Mdet)^2+(P-Pdet)^2+(Mp-Mpdet)^2+(Mb-Mbdet)^2)
  return(resids)
}

