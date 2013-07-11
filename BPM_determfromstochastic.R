  BPMdeterm <- function(parms,k,r,m,h,B.init,P.init,M.init,Mp.init,Mb.init,times){
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
  Predprob<- rep(0,length(times)-1)
  Est<- rep(0,length(times)-1)
  M <- rep(0,length(times))
  Mp <- rep(0,length(times))
  Mb<- rep(0,length(times))
  B <- rep(0,length(times))
  P <- rep(0,length(times))
  M[1] <- M.init
  Mp[1] <- Mp.init
  Mb[1] <- Mb.init
  B[1] <- B.init
  P[1] <- P.init

  Pollprob <- 1-exp(-aPM*h) #pollination
  Dispprob <- 1-exp(-aBM*h) #fruit dispersal
  Flconprob <- 1-exp(-cMP*h)
  Frconprob <- 1-exp(-cMB*h)
  Pollconprob <- 1-exp(-cPB*h)
  DeMprob <- 1-exp(-deM*h) #Mistletoe death
  DeMpprob <- 1-exp(-deMp*h)
  DeMbprob <- 1-exp(-deMb*h)
  DeBprob <- 1-exp(-deB*h)
  DePprob <- 1-exp(-deP*h)
  Predprob<- 1-exp(-aBP*h)
  Matprob <- 1-exp(-m*h)
  for (time in times[1:(length(times)-1)]){
  
  Poll[time] <- Mp[time]*Pollprob
  Disp[time] <- Mb[time]*Dispprob
  Flcon[time] <-  Poll[time]*Flconprob #flower conversion to polls
  Frcon[time] <-  Disp[time]*Frconprob  #fruit conversion to birs
  Pred[time] <- P[time]*Predprob
  Pollcon[time]<-  Pred[time]*Pollconprob #pols conversion to birds
  DeM[time] <- M[time]*DeMprob
  DeMp[time] <- (Mp[time]-Poll[time])*DeMpprob #flower decay
  DeMb[time] <- (Mb[time]-Disp[time])*DeMbprob #fruit decay
  DeB[time] <- B[time]*DeBprob #bird death
  DeP[time] <- (P[time]-Pred[time])*DePprob #pol death
  Fl[time] <- r*M[time] #flowers formed 
  Mat[time] <- Poll[time]*Matprob #flower mature into fruit
  Est[time] <- Disp[time]*(1-M[time]/k)
  
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
  return(cbind(M,B,P,Mp,Mb))
}