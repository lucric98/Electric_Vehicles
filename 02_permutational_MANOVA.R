library(dplyr)
library(ggplot2)
path <- "/Users/luca/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Nonparam Project/DATASET/evdatawithprices.csv"
Vehicles <- read.csv(path,sep = ";")

Vehicles$Seats <- as.factor(Vehicles$Seats)
Vehicles$Charge.Power <- as.factor(Vehicles$Charge.Power)
Vehicles$Drive <- as.factor(Vehicles$Drive)
Vehicles$Available <- as.factor(Vehicles$Available)

factors <- data.frame(Seats = Vehicles$Seats, Charge.Power = Vehicles$Charge.Power, Drive = Vehicles$Drive)

Vehicles <- select_if(Vehicles, is.numeric)
Vehicles <- Vehicles %>% select(-c(id))

data <- as.data.frame(cbind(ACC = Vehicles$Acceleration.0...100.km.h, LENGTH = Vehicles$Length, HEIGHT = Vehicles$Height, PAYLOAD = Vehicles$Max..Payload, CARGO_VOL = Vehicles$Cargo.Volume,
                            RANGE = Vehicles$Electric.Range, 
                            CHARGE_SPEED = Vehicles$Charge.Speed, BATTERY_CAPACITY = Vehicles$Battery.Capacity, FASTCHARGE_SPEED = Vehicles$Fastcharge.Speed,
                            PRICE = Vehicles$Price, CONSUMPTION = Vehicles$Consumption, 
                            POWER = Vehicles$Total.Power))

Vehicles <- Vehicles %>% mutate(factors) 
#riaggiungo le variabili categoriche
data <- data %>% mutate(factors)

########### HIERARCHICAL CLUSTERING
data <- na.omit(data)
library(mvtnorm)
library(rgl)
library(car)

data.e <- daisy(data,metric = "gower")
data.ec <- hclust(data.e, method='complete')
# 3 cluster
cluster.ec3 <- cutree(data.ec,k=3)
clustering <- factor(cluster.ec3, labels=c("1","2","3"))

cl1 <- subset(data[,1:12], cluster.ec3==1)
cl2 <- subset(data[,1:12], cluster.ec3==2)
cl3 <- subset(data[,1:12], cluster.ec3==3)

n1 <- dim(cl1)[1]
n2 <- dim(cl2)[1]
n3 <- dim(cl3)[1]

n  <- n1+n2+n3
g <- 3
p <- 12

#permutational manova
set.seed(seed)
T_stat <- numeric(B)

for(perm in 1:B){
  # choose random permutation
  permutation <- sample(1:n)
  data.perm <- species.name[permutation]
  fit.perm <- manova(as.matrix(iris4) ~ species.name.perm)
  T_stat[perm] <- -summary.manova(fit.perm,test="Wilks")$stats[1,2]
}

# furgoni vs non furgoni
cluster.ec2 <- cutree(data.ec,k=2) 

B = 1000
seed = 26111992

## VOGLIO CONFRONTARE IL PERMUTATIONAL MANOVA CONSIDERANDO NON UN FATTORE MA DUE: PRIMA IL FATTORE Drive e poi il Grouping introdotto dal clustering e ne osserviamo le differenze
############ PERMUTATIONAL ANOVA --> 2 CLUSTER

############ PERMUTATIONAL ANOVA --> 3 CLUSTER

