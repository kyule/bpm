rm(list = ls()) # Removes all objects from the console.
set.seed(101)
# Model for population growth of bird, pollinator mistletoe (flowers and fruit explicit)
# Continuous-time model can be solved with an ODE solver like deSolve.

gen = 30 # Sets the length of the simulation

B.init = 1 # A set of initial values 
P.init = 1
M.init = 1
Mp.init= 0
Mb.init= 0

#Parameter values

k=1000  #total no. inhabitable sites
aBM=0.1 #attack rate of birds on berries
aPM=0.1 #attack rate of pollinators on flowers
aBP=0.1 #attack rate of birds on pollinators
aBB=0.1 #interference competition between birds, territoriality
cMB=0.001 #conversion of fruit to birds
cMP=0.001 #conversion of flowers to pollinators
cPB=0.01 #conversion of pollinators to birds
r=500 #avg. no flowers per female mistletoe
m=0.05 #flower to fruit conversion rate
deP= 0.01 #extrinsic death rate of pollinators
deB= 0.01 #extrinsic death rate of birds
deM= 0.01 #extrinisic death rate of mistletoe
deMp=0.1 #decay of unpollinated flowers
deMb=0.1 #decay of uneaten fruit

# The parameters are then loaded into a vectors drawing from the values defined above.
parameters = c(aBM =aBM,
               aPM=aPM,
               aBP=aBP,
               aBB=aBB,
               deP=deP,
               deB=deB,
               deM=deM,
               deMb=deMb,
               deMp=deMp,
               r=r,
               m=m,
               k=k
)

# The initial conditions are loaded into a vector named "state" for the initial state.
state = c(Mp = Mp.init,
          Mb = Mb.init,
          M = M.init,
          B = B.init,
          P = P.init
)

Interactions=function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    dMp = M*r-Mp*aPM*P-deMp*Mp
    dMb = Mp*aPM*P*m-Mb*aBM*B-deMb*Mb
    dM = Mb*aBM*B*(1-M/k)-deM*M
    dB = B*(aBM*Mb*cMB+aBP*cPB*P)-aBB*B*B-deB*B
    dP = aPM*cMP*P*Mp-aBP*B*P-deP*P
    list(c(dMp,dMb,dM,dB,dP))
  })
}
# Gives a sequence over which time is defined (0 to gen by 1's)
times = seq(0, gen, by = 1)

# Calling up the package "deSolve" to use to solve this inital value problem.
library(deSolve)

# "out" is the output of the function "lsoda" that solves the inital value ODE.
# You can see that it requires an set of initial values, y, the time over which to 
# integrate, times, the set of differential equations, func, and the parameters used
# in the model, parms. This produces a matrix output, labelled here as out, that has 
# 0:gen number of rows and columns that show time and the abundance of all four species
# individually.
out = lsoda(y = state, times = times, func = Interactions, parms = parameters)

# This command shows only the first few rows of the matrix and its headings. 
# Its really useful when you have a really large matrix and just want to check to 
# make sure it is operating the way you want it to.
head(out)


quartz() # Quartz opens a figure window in mac. The equivalent command is x11() on a
# windows based machine.

# Here I am plotting the column M (resource 1 abundance) against the time column 
# and defining the yaxis range, making it a line, and labelling the axes.
# The points commands add lines to an existing figure.
# The legend command puts the legend in the 'topright' corner for the figure.
plot(out[, 'time'], out[, 'M'], ylim = c(0, max(out[, 'B'], out[, 'P'], out[, 'M'])), typ = 'l', ylab = 'Population Density', xlab = 'Time')
points(out[, 'time'], out[, 'B'], col = 'red', typ = 'l')
points(out[, 'time'], out[, 'P'], col = 'blue', typ = 'l')
legend(x = 'topright', bty='n',legend = c('Mistletoe', 'Birds', 'Pollinators'), col = c('black', 'red', 'blue'), lwd = 2)


# Plotting the same dynamics on a log scale.
quartz()
plot(out[, 'time'], log(out[, 'M']), ylim = c(min(log(out[, 'B']), log(out[, 'P']), log(out[, 'M'])), max(log(out[, 'B']), log(out[, 'P']), log(out[, 'M']))), typ = 'l', ylab = 'ln(Population Size)', xlab='Time')
points(out[, 'time'], log(out[, 'B']), col = 'red', typ = 'l')
points(out[, 'time'], log(out[, 'P']), col = 'blue', typ = 'l')
legend(x = 'topright', legend = c('M', 'B', 'P'), col = c('black', 'red', 'blue'), lwd = 2)
out[30,]
