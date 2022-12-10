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
  return(p_val)
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
    T_stat[perm] <- summary(fit.perm)[[1]][1,4]
  }
  
  hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
  abline(v=T0,col=3,lwd=2)
  
  plot(ecdf(T_stat))
  abline(v=T0,col=3,lwd=4)
  
  # p-value
  p_val <- sum(T_stat>=T0)/B
  return(p_val)
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
## permutational Manova
perm_manova(clustering.m2)
# permutational p-value = 0

# my_xlab <- paste(levels(data$Type),"\n(N=",table(data$Type),")",sep="")
# 
# ggplot(data, aes(x=Type, y=HEIGHT, fill=Type)) +
#   geom_boxplot(varwidth = TRUE, alpha=0.2) +
#   theme(legend.position="none") +
#   scale_x_discrete(labels=my_xlab)

for(i in 1:12){
  print(perm_anova(data[,i],clustering.m2))
}
# PERMUTATIONAL P-VALUE
# [1] 0
# [1] 0
# [1] 0
# [1] 0
# [1] 0
# [1] 0
# [1] 0
# [1] 0.0024
# [1] 0
# [1] 0.0835 --> PRICE unica caratteristica per cui accettiamo l'ipotesi nulla
# [1] 0
# [1] 0

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
#permutational Manova
perm_manova(clustering.w3)
# permutational p-value = 0

#permutational Anova
for(i in 1:12){
  print(perm_anova(data[,i],clustering.w3))
}
# PERMUTATIONAL P-VALUE 
# [1] 2e-04
# [1] 0
# [1] 0
# [1] 0
# [1] 0
# [1] 0.0309
# [1] 0
# [1] 0.0038
# [1] 0.6742 -> FASTCHARGE_SPEED
# [1] 0
# [1] 0
# [1] 0.0966 -> POWER

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
#permutational Manova
perm_manova(clustering.m3)
# permutational p-value = 0

#permutational Anova
for(i in 1:12){
  print(perm_anova(data[,i],clustering.m3))
}
# PERMUTATIONAL P-VALUE 
# [1] 0
# [1] 0.2779 -> LENGTH
# [1] 0
# [1] 0
# [1] 0
# [1] 0
# [1] 0
# [1] 0
# [1] 0
# [1] 0
# [1] 0
# [1] 0

### PERMUTATIONAL MANOVA PER LA TRAZIONE
table(data$Drive)
perm_manova(data$Drive)
# permutational p-value = 0

perm_anova(data$ACC,data$Drive)

## PLOT DELLE QUANTITA' DI INTERESSE RISPETTO ALLE VARIA SUDDIVISIONI
par(mfrow=c(2,2))
plot(data$PRICE, main="Drive", col = rainbow(data$Drive),pch=16)
plot(data$PRICE, main="2 clusters, mcquitty linkage", col = clustering.m2,pch=16)
plot(data$PRICE, main="3 clusters, mcquitty linkage", col = clustering.m3,pch=16)
plot(data$PRICE, main="3 clusters, ward linkage", col = clustering.w3,pch=16)

par(mfrow=c(2,2))
plot(data$RANGE, main="Drive", col = rainbow(data$Drive),pch=16)
plot(data$RANGE, main="2 clusters, mcquitty linkage", col = clustering.m2,pch=16)
plot(data$RANGE, main="3 clusters, mcquitty linkage", col = clustering.m3,pch=16)
plot(data$RANGE, main="3 clusters, ward linkage", col = clustering.w3,pch=16)

par(mfrow=c(2,2))
plot(data$CONSUMPTION, main="Drive", col = rainbow(data$Drive),pch=16)
plot(data$CONSUMPTION, main="2 clusters, mcquitty linkage", col = clustering.m2,pch=16)
plot(data$CONSUMPTION, main="3 clusters, mcquitty linkage", col = clustering.m3,pch=16)
plot(data$CONSUMPTION, main="3 clusters, ward linkage", col = clustering.w3,pch=16)


perm_anova(data$PRICE,clustering.m3)
summary(aov(data$HEIGHT~clustering.m2 ))
library(GGally)
data[,1:12] <- scale(data[,1:12])
data_clusterpairs<- cbind(data,clustering.m2)
colnames(data_clusterpairs)[16] <- c("C2")
data_clusterpairs$C2<-factor(data_clusterpairs$C2, levels = 1:2, labels = c("Car","Van") )
data_clusterpairs <- data_clusterpairs[,c(3,5,10,11,16)]
colnames(data_clusterpairs)<-c("Height","Cargo_Vol","Price","Consumption","Cluster")
ggpairs(data_clusterpairs[,1:4], aes(col=data_clusterpairs[,5])) + theme(text=element_text(size=15))

data_clusterplot<- cbind(data,clustering.w3)
colnames(data_clusterplot)[16] <- c("C3")
data_clusterplot$C3<-factor(data_clusterplot$C3, levels = 1:3, labels = c("K1","K2","K3") )
ggplot(data = data_clusterplot, aes(x=data_clusterplot$ACC, y=data_clusterplot$HEIGHT,col = data_clusterplot$C3)) +
  geom_point(size = 4) + theme(text=element_text(size=40)) + labs(colour = "Cluster")+
  labs(x = "Acceleration") + labs(y = "Height")


