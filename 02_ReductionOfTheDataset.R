
library(GGally)
# ggpairs per fare plot dei pair carini con colori
# una domanda di ricerca interessante potrebbe essere se le
# macchine tedesche sul mercato tedesco costano meno 
# (a parità di modello e caratteristiche con altre)

setwd("~/Downloads/Nonparam Project")
auto<-read.csv("completedataset.csv", sep=";")

auto<-auto[,-1]

## WEATHER CONDITIONS ##

ggpairs(auto[,2:7])

# the data about weather have a stroooong correlation (>0.99); 
# we can keep just one of them as a summary of the information

auto<-auto[,-c(2,3,4,5,6)]

## PHYSICAL DIMENSIONS ##

ggpairs(auto[,13:19])

# length and width correlated (0.86), even length and wheelbase (0.9)
# and length and GVWR (0.9); so we can just jeep length as a summary of all these variables

auto<-auto[,-c(14,16,17)]

ggpairs(auto[,13:16])

# in all these plots we see that the correlation is quite strong till an extremum 
# (that is the group with max.payload>800). Checking we see that thi group is composed only of vans,
# and we can see it because those are the vehicles with Consumption higher than 250

auto[which(autofordept$Max..Payload>800),19]

# so we keep as a summary of the physical quantities Length, Height, Volume and max payload,
# that seems to be good to describe basically all the vehicle 

## ALL THE OTHERS ##

# I delete covariates link and availability (useless)

auto<-auto[,-c(21, 22)]

ggpairs(auto[,-c(1, 8)])

# we check all remaining covariates at once (excluding the categorical ones);
# we see clearly that Combined...Mild.Weather, Eletric.Range and Battery Capacity
# are basically the same thing

auto<-auto[,-c(2, 5)]

ggpairs(auto[,-c(1, 6)])

# we also delete comsumption (same as fuel.equivalent)

auto<-auto[,-17]

# we also delete charge.power (same as charge.speed)

auto<-auto[,-8]

ggpairs(auto[,-c(1, 6)])

# total power summarizes top speed and total torque very well

auto<-auto[,-c(3, 5)]

# also acceleration can be deleted, because basically 
# (inversely) proportional to total power, as the intuition would suggest

auto<-auto[,-2]

ggpairs(auto[,-c(1, 3)])

# finally, the seats covariate is highly correlated to some physical features;
# for the moment however we keep it pecause of its importance and peculiarity

