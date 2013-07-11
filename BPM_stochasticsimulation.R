
set.seed(101)
# Simulate stochastic population dynamics of bird, pollinator mistletoe (flowers and fruit explicit)

#Parameter values

k=1000  #total no. inhabitable sites
aBM=0.3 #attack rate of birds on berries
aPM=0.5 #attack rate of pollinators on flowers
aBP=0.1 #attack rate of birds on pollinators
cMB=0.0001 #conversion of fruit to birds
cMP=0.01 #conversion of flowers to pollinators
cPB=0.01 #conversion of pollinators to birds
r=500 #mean no flowers per female mistletoe
rsd=50 #sd of number of flowers per female mistletoe
m=0.05 # mean flower to fruit conversion rate
deP= 0.01 #extrinsic death rate of pollinators
deB= 0.001 #extrinsic death rate of birds
deM= 0.001 #extrinisic death rate of mistletoe
deMp=0.8 #decay of unpollinated flowers
deMb= 0.5 #decay of uneaten fruit
h=1 #observation time step

#Initiate time series
times <- seq(1,10000,h)

#Initial conditions
M.init <- 100
B.init <- 10
P.init <- 100
Mp.init <- 100
Mb.init <- 100
  
#Initiate State Variables  

M <- rep(0,length(times))
B <- rep(0,length(times))
P <- rep(0,length(times))
Mp <- rep(0,length(times))
Mb <- rep(0,length(times))
M[1] <- M.init
B[1] <- B.init
P[1] <- P.init
Mp[1] <- Mp.init
Mb[1] <- Mb.init

#Initiate Transitions 
Fl <- rep(0,length(times)-1) #from mistletoe to mistletoe flowers
Poll <- rep(0,length(times)-1) #pollination
Disp <- rep(0,length(times)-1) #fruit dispersal
Flcon <-  rep(0,length(times)-1) #flower conversion to polls
Frcon <-  rep(0,length(times)-1) #fruit conversion to birs
Pollcon <-  rep(0,length(times)-1) #pols conversion to birds
Pred <- rep(0,length(times)-1) #pol pred
DeM <- rep(0,length(times)-1) #Mistletoe death
DeMp <- rep(0,length(times)-1) #flower decay
DeMb <- rep(0,length(times)-1) #fruit decay
DeB <- rep(0,length(times)-1) #bird death
DeP <- rep(0,length(times)-1) #pol death
Fl <- rep(0,length(times)-1) #flowers formed per mistletoe
Mat <- rep(0,length(times)-1) #flower mature into fruit
Dispprob<- rep(0,length(times)-1)
Pollprob<- rep(0,length(times)-1)
Predprob<- rep(0,length(times)-1)
Est<- rep(0,length(times)-1)

for (time in times[1:(length(times)-1)])
{
  ###Think about whether it makes sense to have the state variables in the exp in this situaiton
  Pollprob[time] <- 1-exp(-aPM*h) #pollination
  Poll[time] <- rbinom(1,Mp[time],Pollprob[time])
 Dispprob[time] <- 1-exp(-aBM*h) #fruit dispersal
 Disp[time] <- rbinom(1,Mb[time],Dispprob[time])
  Flconprob <- 1-exp(-cMP*h)
  Frconprob <- 1-exp(-cMB*h)
  Pollconprob <- 1-exp(-cPB*h)
  Flcon[time] <-  rbinom(1,Poll[time],Flconprob) #flower conversion to polls
  Frcon[time] <-  rbinom(1,Disp[time],Frconprob)  #fruit conversion to birs
  Predprob[time] <- 1-exp(-aBP*h)
  Pred[time] <- rbinom(1,P[time],Predprob[time]) #pol pred
  Pollcon[time]<-  rbinom(1,Pred[time],Pollconprob)  #pols conversion to birds
  DeMprob <- 1-exp(-deM*h) #Mistletoe death
  DeMpprob <- 1-exp(-deMp*h)
  DeMbprob <- 1-exp(-deMb*h)
  DeBprob <- 1-exp(-deB*h)
  DePprob <- 1-exp(-deP*h)
  DeM[time] <- rbinom(1,M[time],DeMprob)
  DeMp[time] <- rbinom(1,(Mp[time]-Poll[time]),DeMpprob) #flower decay
  DeMb[time] <- rbinom(1,(Mb[time]-Disp[time]),DeMbprob) #fruit decay
  DeB[time] <- rbinom(1,B[time],DeBprob) #bird death
  DeP[time] <- rbinom(1,(P[time]-Pred[time]),DePprob) #pol death
  Fl[time] <- sum(round(rnorm(M[time],r*h,rsd*h))) #flowers formed 
  Matprob <- 1-exp(-m*h)
  Mat[time] <- rbinom(1,Poll[time],Matprob)#flower mature into fruit
  Est[time] <- rbinom(1,Disp[time],(1-M[time]/k))
 
  
  #Flowers=number before-number decayed-number pollinated+number formed
  Mp[time+1] <- Mp[time]-DeMp[time]-Poll[time]+Fl[time]
  #Fruit=number before-number decayed-number dispersed+number formed
  Mb[time+1] <- Mb[time]-DeMb[time]-Disp[time]+Mat[time]
  #Mistletoe=number before +number established-number died
  M[time+1] <- min(k, M[time]+Est[time])-DeM[time]
  #Birds=number before- number died + number born due to fruits eaten + number born due to polls eaten
  B[time+1] <- B[time]-DeB[time]+Frcon[time]+Pollcon[time]
  #Pollinators= number before + number born due to flowers pollinated -number died- number eaten
  P[time+1] <- P[time]+Flcon[time]-DeP[time]-Pred[time]
}
plot(times,M,col="red")
plot(times,B,col="blue")
plot(times,P,col="green4")