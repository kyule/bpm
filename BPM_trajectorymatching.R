rm(list = ls()) # Removes all objects from the console.
setwd("~/Desktop/bpm")
source("BPM_stochasticsimulation.R")
#rm(aBM,aPM,aBP,cMB,cMP,cPB,deP,deB,deM,deMp,deMb)
source("BPM_determfromstochastic.R")
source("BPM_determleastsquares.R")

times <- seq(1,10000,h)

#Parameter guesses
aBMinit=0.5
aPMinit=0.5
aBPinit=0.5
cMPinit=0.005
cMBinit=0.1
cPBinit=0.1
dePinit=1
deBinit=1
deMinit=0.5
deMpinit=0.5
deMbinit=0.5

#Optimize parameters in deterministic model for data generated using the stochastic model
TM <- optim(c(aBMinit,aPMinit,aBPinit,cMBinit,cMPinit,cPBinit,dePinit,deBinit,deMinit,deMpinit,deMbinit),BPM_determresids,M=M,P=P,B=B,Mp=Mp,Mb=Mb,k=k,r=r,m=m,h=h,B.init=B.init,P.init=P.init,M.init=M.init,Mp.init=Mp.init,Mb.init=Mb.init,times=times)
TM$par

library(bbmle)
