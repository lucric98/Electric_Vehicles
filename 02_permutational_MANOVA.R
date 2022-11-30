library(dplyr)
library(ggplot2)
path <- "/Users/luca/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Nonparam Project/DATASET/evdatawithprices.csv"
Vehicles <- read.csv(path,sep = ";")

Vehicles$Seats <- as.factor(Vehicles$Seats)
Vehicles$Charge.Power <- as.factor(Vehicles$Charge.Power)
Vehicles$Drive <- as.factor(Vehicles$Drive)
Vehicles$Available <- as.factor(Vehicles$Available)

factors <- data.frame(Seats = Vehicles$Seats, Charge.Power = Vehicles$Charge.Power, Drive = Vehicles$Drive)

data <- as.data.frame(cbind(ACC = Vehicles$Acceleration.0...100.km.h, LENGTH = Vehicles$Length, HEIGHT = Vehicles$Height, PAYLOAD = Vehicles$Max..Payload, CARGO_VOL = Vehicles$Cargo.Volume,
                            RANGE = Vehicles$Electric.Range, 
                            CHARGE_SPEED = Vehicles$Charge.Speed, BATTERY_CAPACITY = Vehicles$Battery.Capacity, FASTCHARGE_SPEED = Vehicles$Fastcharge.Speed,
                            PRICE = Vehicles$Price, CONSUMPTION = Vehicles$Consumption, 
                            POWER = Vehicles$Total.Power))

Vehicles <- Vehicles %>% mutate(factors) 
#riaggiungo le variabili categoriche
data <- data %>% mutate(factors)

perm_manova <- function(clustering)
{
  set.seed(seed)
  T_stat <- numeric(B)
  
  fit <- manova(as.matrix(data[,1:12]) ~ clustering)
  summary.manova(fit,test="Wilks") 
  T0 <- -summary.manova(fit,test="Wilks")$stats[1,2]
  T0
  
  for(perm in 1:B){
    # choose random permutation
    permutation <- sample(1:n)
    clust.perm <- clustering[permutation]
    fit.perm <- manova(as.matrix(data[,1:12]) ~ clust.perm)
    T_stat[perm] <- -summary.manova(fit.perm,test="Wilks")$stats[1,2]
  }
  
  hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
  abline(v=T0,col=3,lwd=2)
  
  plot(ecdf(T_stat),xlim=c(-2,1))
  abline(v=T0,col=3,lwd=4)
  
  # p-value
  p_val <- sum(T_stat>=T0)/B
  p_val
}

perm_anova <- function(X, clustering)
{
  set.seed(seed)
  T_stat <- numeric(B)
  
  fit <- aov(X ~ clustering)
  T0 <- summary(fit)[[1]][1,4]
  T0
  
  for(perm in 1:B){
    # choose random permutation
    permutation <- sample(1:n)
    X.perm <- X[permutation]
    fit.perm <- aov(X.perm ~ clustering)
    T_stat[perm] <- summary(fit)[[1]][1,4]
  }
  
  hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
  abline(v=T0,col=3,lwd=2)
  
  plot(ecdf(T_stat))
  abline(v=T0,col=3,lwd=4)
  
  # p-value
  p_val <- sum(T_stat>=T0)/B
  p_val
}

########### HIERARCHICAL CLUSTERING
data <- na.omit(data)
library(mvtnorm)
library(rgl)
library(car)
library(cluster)

B = 10000
seed = 26111992

data.e <- daisy(data,metric = "gower")
data.ea <- hclust(data.e, method = "average")
data.em <- hclust(data.e, method = "mcquitty")
data.ew <- hclust(data.e, method = "ward.D")
# clustering
clustering.a2 <- cutree(data.ea,k=2)
clustering.m2 <- cutree(data.em,k=2)

clustering.a3 <- cutree(data.ea,k=3)
clustering.m3 <- cutree(data.em,k=3)
clustering.w3 <- cutree(data.ew,k=3)

### PERMUTATIONAL MANOVA WITH K=2, MCQUITTY LINKAGE
cl1 <- subset(data[,1:12], clustering.m2==1)
cl2 <- subset(data[,1:12], clustering.m2==2)

n1 <- dim(cl1)[1]
n2 <- dim(cl2)[1]

n  <- n1+n2
g <- 2
p <- 12
## Manova
perm_manova(clustering.m2)
# permutational p-value = 0

# my_xlab <- paste(levels(data$Type),"\n(N=",table(data$Type),")",sep="")
# 
# ggplot(data, aes(x=Type, y=HEIGHT, fill=Type)) +
#   geom_boxplot(varwidth = TRUE, alpha=0.2) +
#   theme(legend.position="none") +
#   scale_x_discrete(labels=my_xlab)

# perm_anova(data$ACC,clustering.m2)
# perm_anova(data$HEIGHT, clustering.m2)
# perm_anova(data$LENGTH,clustering.m2)
# perm_anova(data$PAYLOAD,clustering.m2)
### PROBLEMA! vedi file commenti

### PERMUTATIONAL MANOVA WITH K=3, WARD LINKAGE
cl1 <- subset(data[,1:12], clustering.w3==1)
cl2 <- subset(data[,1:12], clustering.w3==2)
cl3 <- subset(data[,1:12], clustering.w3==3)

n1 <- dim(cl1)[1]
n2 <- dim(cl2)[1]
n3 <- dim(cl3)[1]

n  <- n1+n2+n3
g <- 3
p <- 12
perm_manova(clustering.w3)
# permutational p-value = 0
### PERMUTATIONAL MANOVA WITH K=3, MCQUITTY LINKAGE
cl1 <- subset(data[,1:12], clustering.m3==1)
cl2 <- subset(data[,1:12], clustering.m3==2)
cl3 <- subset(data[,1:12], clustering.m3==3)

n1 <- dim(cl1)[1]
n2 <- dim(cl2)[1]
n3 <- dim(cl3)[1]

n  <- n1+n2+n3
g <- 3
p <- 12
perm_manova(clustering.m3)
# permutational p-value = 0

### PERMUTATIONAL MANOVA PER LA TRAZIONE
table(data$Drive)
perm_manova(data$Drive)
# permutational p-value = 0

perm_anova(data$ACC,data$Drive)

## PLOT DELLE QUANTITA' DI INTERESSE RISPETTO ALLE VARIA SUDDIVISIONI
par(mfrow=c(2,2))
plot(data$PRICE, main="Drive", col = rainbow(data$Drive),pch=16)
plot(data$PRICE, main="2 clusters, mcquitty linkage", col = clustering.m2+1,pch=16)
plot(data$PRICE, main="3 clusters, mcquitty linkage", col = clustering.m3+1,pch=16)
plot(data$PRICE, main="3 clusters, ward linkage", col = clustering.w3+1,pch=16)

par(mfrow=c(2,2))
plot(data$RANGE, main="Drive", col = rainbow(data$Drive),pch=16)
plot(data$RANGE, main="2 clusters, mcquitty linkage", col = clustering.m2+1,pch=16)
plot(data$RANGE, main="3 clusters, mcquitty linkage", col = clustering.m3+1,pch=16)
plot(data$RANGE, main="3 clusters, ward linkage", col = clustering.w3+1,pch=16)

par(mfrow=c(2,2))
plot(data$CONSUMPTION, main="Drive", col = rainbow(data$Drive),pch=16)
plot(data$CONSUMPTION, main="2 clusters, mcquitty linkage", col = clustering.m2,pch=16)
plot(data$CONSUMPTION, main="3 clusters, mcquitty linkage", col = clustering.m3,pch=16)
plot(data$CONSUMPTION, main="3 clusters, ward linkage", col = clustering.w3,pch=16)


#perm_anova(data$PRICE,clustering.m3)
