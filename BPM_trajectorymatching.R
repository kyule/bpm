rm(list = ls()) # Removes all objects from the console.
setwd("~/Desktop/bpm")
source("BPM_stochasticsimulation.R")
#rm(aBM,aPM,aBP,cMB,cMP,cPB,deP,deB,deM,deMp,deMb)
source("BPM_determfromstochastic.R")
source("BPM_determleastsquares.R")

times <- seq(1,10000,h)

#Parameter guesses
aBMinit=0.1
aPMinit=0.1
aBPinit=0.1
cMPinit=0.01
cMBinit=0.01
cPBinit=0.01
dePinit=0.1
deBinit=0.1
deMinit=0.1
deMpinit=0.1
deMbinit=0.1

#Optimize parameters in deterministic model for data generated using the stochastic model
TM <- optim(c(aBMinit,aPMinit,aBPinit,cMBinit,cMPinit,cPBinit,dePinit,deBinit,deMinit,deMpinit,deMbinit),BPM_determresids,M=M,P=P,B=B,Mp=Mp,Mb=Mb,k=k,r=r,m=m,h=h,B.init=B.init,P.init=P.init,M.init=M.init,Mp.init=Mp.init,Mb.init=Mb.init,times=times)
TM$par