
library(ggplot2)
library(DepthProc)
library(aplpack)
library(corrplot)
library(GGally)
library(mvtnorm)
library(rgl)
library(car)
library(cluster)
library(dplyr)
library(ISLR2)
library(mgcv)
library(splines)
library(pbapply)

Vehicles = evdatawithprices
Vehicles$Seats <- as.factor(Vehicles$Seats)
Vehicles$Charge.Power <- as.factor(Vehicles$Charge.Power)
Vehicles$Drive <- as.factor(Vehicles$Drive)
Vehicles$Available <- as.factor(Vehicles$Available)

### BOXPLOTS

#better visualization
ggplot(data = Vehicles, aes(x = Drive, y = Price, fill = Drive)) + 
  geom_boxplot() + 
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Box Plot of Different Drive") +
  labs(x = "Drive", y = "Price") + 
  theme_classic()

boxplot(Vehicles$Combined...Cold.Weather)
which(Vehicles$Combined...Cold.Weather %in% boxplot.stats(Vehicles$Combined...Cold.Weather)$out)

boxplot(Vehicles$Combined...Mild.Weather)
which(Vehicles$Combined...Mild.Weather %in% boxplot.stats(Vehicles$Combined...Mild.Weather)$out)

boxplot(Vehicles$Total.Power)

boxplot(Vehicles$Total.Torque)

boxplot(Vehicles$Acceleration.0...100.km.h)
which(Vehicles$Acceleration.0...100.km.h %in% boxplot.stats(Vehicles$Acceleration.0...100.km.h)$out)

boxplot(Vehicles$Top.Speed)

boxplot(Vehicles$Length)
which(Vehicles$Length %in% boxplot.stats(Vehicles$Length)$out)

boxplot(Vehicles$Width)
which(Vehicles$Width %in% boxplot.stats(Vehicles$Width)$out)

boxplot(Vehicles$Height)
which(Vehicles$Height %in% boxplot.stats(Vehicles$Height)$out)

boxplot(Vehicles$Cargo.Volume)
which(Vehicles$Cargo.Volume %in% boxplot.stats(Vehicles$Cargo.Volume)$out)

boxplot(Vehicles$Gross.Vehicle.Weight..GVWR.)
which(Vehicles$Gross.Vehicle.Weight..GVWR. %in% boxplot.stats(Vehicles$Gross.Vehicle.Weight..GVWR.)$out)

boxplot(Vehicles$Battery.Capacity)

boxplot(Vehicles$Charge.Speed)
which(Vehicles$Charge.Speed %in% boxplot.stats(Vehicles$Charge.Speed)$out)

boxplot(Vehicles$Fastcharge.Speed)
which(Vehicles$Fastcharge.Speed %in% boxplot.stats(Vehicles$Fastcharge.Speed)$out)

boxplot(Vehicles$Consumption)
which(Vehicles$Consumption %in% boxplot.stats(Vehicles$Consumption)$out)

boxplot(Vehicles$Vehicle.Fuel.Euivalent)
which(Vehicles$Vehicle.Fuel.Euivalent %in% boxplot.stats(Vehicles$Vehicle.Fuel.Euivalent)$out)

boxplot(Vehicles$Price)
which(Vehicles$Price %in% boxplot.stats(Vehicles$Price)$out)

#####################################################################################

#bagplot
df <- cbind(Vehicles$Height,Vehicles$Length)
bagplot(df, main = "Boxplot (Length vs Height)", ylab = "Length", xlab = "Height")
depthContour(df,depth_params = list(method='Tukey'))

#bagplot
df <- cbind(data$RANGE,data$LENGTH)
bagplot(df, ylab = "Range", xlab = "Length")
depthContour(df,depth_params = list(method='Tukey'))

#bagplot
df <- cbind(data$CONSUMPTION,data$POWER)
bagplot(df, ylab = "Consumption", xlab = "Power")
depthContour(df,depth_params = list(method='Tukey'))

#######################################################################################

data <- as.data.frame(cbind(ACC = Vehicles$Acceleration.0...100.km.h, LENGTH = Vehicles$Length, HEIGHT = Vehicles$Height, PAYLOAD = Vehicles$Max..Payload, CARGO_VOL = Vehicles$Cargo.Volume,
                            RANGE = Vehicles$Electric.Range, 
                            CHARGE_SPEED = Vehicles$Charge.Speed, BATTERY_CAPACITY = Vehicles$Battery.Capacity, FASTCHARGE_SPEED = Vehicles$Fastcharge.Speed,
                            PRICE = Vehicles$Price, CONSUMPTION = Vehicles$Consumption, 
                            POWER = Vehicles$Total.Power))
pairs(data)
bagplot.pairs(data)

factors <- data.frame(Seats = Vehicles$Seats, Charge.Power = Vehicles$Charge.Power, Drive = Vehicles$Drive)
data <- data %>% mutate(factors)

ggpairs(data,
        columns = 1:12,
        aes(color = Drive, alpha = 0.5), 
        upper = list(continuous = wrap("cor", size = 2.5)))
ggpairs(data[,1:15],
        aes(color = Drive, alpha = 0.5), 
        lower = list(combo = "count"))

#########################################################################################

data <- na.omit(data)
data.e <- daisy(data,metric = "gower")
data.em <- hclust(data.e, method = "mcquitty")
data.ew <- hclust(data.e, method = "ward.D")
clustering.m3 <- as.factor(cutree(data.em,k=3))
clustering.w3 <- as.factor(cutree(data.ew,k=3))
clustering.m2 <- as.factor(cutree(data.em,k=2))
data.m2 <- data %>% mutate(clustering.m2)
data.m3 <- data %>% mutate(clustering.m3)
data.w3 <- data %>% mutate(clustering.w3)

##########################################################################################

pairs(cbind(PRICE = data$PRICE, ACC = data$ACC, LENGTH = data$LENGTH, HEIGHT = data$HEIGHT, RANGE = data$RANGE, POWER = data$POWER, BATTERY_CAPACITY = data$BATTERY_CAPACITY, CARGO_VOL = data$CARGO_VOL, CONSUMPTION = data$CONSUMPTION), col = data$Drive, pch=16, main = "Drive")
pairs(cbind(PRICE = data$PRICE, ACC = data$ACC, LENGTH = data$LENGTH, HEIGHT = data$HEIGHT, RANGE = data$RANGE, POWER = data$POWER, BATTERY_CAPACITY = data$BATTERY_CAPACITY, CARGO_VOL = data$CARGO_VOL, CONSUMPTION = data$CONSUMPTION), col = clustering.m2, pch=16, main = "VAN vs CAR")
pairs(cbind(PRICE = data$PRICE, ACC = data$ACC, LENGTH = data$LENGTH, HEIGHT = data$HEIGHT, RANGE = data$RANGE, POWER = data$POWER, BATTERY_CAPACITY = data$BATTERY_CAPACITY, CARGO_VOL = data$CARGO_VOL, CONSUMPTION = data$CONSUMPTION), col = clustering.m3, pch=16, main = "3 clusters, Mcquitty linkage")
pairs(cbind(PRICE = data$PRICE, ACC = data$ACC, LENGTH = data$LENGTH, HEIGHT = data$HEIGHT, RANGE = data$RANGE, POWER = data$POWER, BATTERY_CAPACITY = data$BATTERY_CAPACITY, CARGO_VOL = data$CARGO_VOL, CONSUMPTION = data$CONSUMPTION), col = clustering.w3, pch=16, main = "3 clusters, Ward linkage")
for (i in 1:12){
  ##check visivi per capire le relazioni tra i vari punti del dataset
  pairs(cbind(data$PRICE,data[,i]), main = colnames(data)[i])
  bagplot.pairs(cbind(data$PRICE,data[,i]), main = colnames(data)[i])
  ## voglio capire se ci siano variabili per cui una relazione lineare è sufficiente o è necessario modellare nonlinearmente tutte le covariate
  with(data, scatterplotMatrix(data.frame(data$PRICE, data[,i])))
  ## variabili che proviamo a trattare con modello lineare: FASTCHARGE_SPEED, POWER, RANGE, LENGTH
}

scaled_data <- data
scaled_data[,1:12] <- scale(data[,1:12])

gam_PRICE2 <- gam(PRICE ~ s(ACC, bs="cr") + s(HEIGHT, bs="cr"), data=scaled_data)

gam_PRICE3 <- gam(PRICE ~ s(LENGTH, bs="cr") + s(ACC,bs="cr"), data=scaled_data)

ACC.grid=seq(range(scaled_data$ACC)[1],range(scaled_data$ACC)[2],length.out = 100)
HEIGHT.grid=seq(range(scaled_data$HEIGHT)[1],range(scaled_data$HEIGHT)[2],length.out = 100)
LENGTH.grid=seq(range(scaled_data$LENGTH)[1],range(scaled_data$LENGTH)[2],length.out = 100)

grid_ACC_HEIGHT=expand.grid(ACC.grid,HEIGHT.grid)
names(grid_ACC_HEIGHT)=c('ACC','HEIGHT')

grid_ACC_LENGTH=expand.grid(ACC.grid,LENGTH.grid)
names(grid_ACC_LENGTH)=c('ACC','LENGTH')

grid_HEIGHT_LENGTH=expand.grid(HEIGHT.grid,LENGTH.grid)
names(grid_HEIGHT_LENGTH)=c('HEIGHT','LENGTH')

pred_gam_PRICE2=predict(gam_PRICE2,newdata=grid_ACC_HEIGHT)
persp3d(ACC.grid,HEIGHT.grid,pred_gam_PRICE2,col='grey30')
points3d(scaled_data$ACC,scaled_data$HEIGHT,scaled_data$PRICE,col=clustering.m3,size=5)
#points3d(scaled_data$ACC,scaled_data$HEIGHT,scaled_data$PRICE,col=clustering.m2,size=5)
#points3d(scaled_data$ACC,scaled_data$HEIGHT,scaled_data$PRICE,col=clustering.w3,size=5)

pred_gam_PRICE3=predict(gam_PRICE3,newdata=grid_ACC_LENGTH)
persp3d(ACC.grid,LENGTH.grid,pred_gam_PRICE3,col='grey30')
points3d(scaled_data$ACC,scaled_data$LENGTH,scaled_data$PRICE,col=clustering.m3,size=5)
#points3d(scaled_data$ACC,scaled_data$LENGTH,scaled_data$PRICE,col=clustering.m2,size=5)
#points3d(scaled_data$ACC,scaled_data$LENGTH,scaled_data$PRICE,col=clustering.w3,size=5)

##########################################################################################

pairs(cbind(CONSUMPTION = data$CONSUMPTION, ACC = data$ACC, LENGTH = data$LENGTH, HEIGHT = data$HEIGHT, RANGE = data$RANGE, POWER = data$POWER, BATTERY_CAPACITY = data$BATTERY_CAPACITY, CARGO_VOL = data$CARGO_VOL, PRICE = data$PRICE), col = data$Drive, pch=16, main = "Drive")
pairs(cbind(CONSUMPTION = data$CONSUMPTION, ACC = data$ACC, LENGTH = data$LENGTH, HEIGHT = data$HEIGHT, RANGE = data$RANGE, POWER = data$POWER, BATTERY_CAPACITY = data$BATTERY_CAPACITY, CARGO_VOL = data$CARGO_VOL, PRICE = data$PRICE), col = clustering.m2, pch=16, main = "VAN vs CAR")
pairs(cbind(CONSUMPTION = data$CONSUMPTION, ACC = data$ACC, LENGTH = data$LENGTH, HEIGHT = data$HEIGHT, RANGE = data$RANGE, POWER = data$POWER, BATTERY_CAPACITY = data$BATTERY_CAPACITY, CARGO_VOL = data$CARGO_VOL, PRICE = data$PRICE), col = clustering.m3, pch=16, main = "3 clusters, Mcquitty linkage")
pairs(cbind(CONSUMPTION = data$CONSUMPTION, ACC = data$ACC, LENGTH = data$LENGTH, HEIGHT = data$HEIGHT, RANGE = data$RANGE, POWER = data$POWER, BATTERY_CAPACITY = data$BATTERY_CAPACITY, CARGO_VOL = data$CARGO_VOL, PRICE = data$PRICE), col = clustering.w3, pch=16, main = "3 clusters, Ward linkage")
for (i in 1:12){
  ##check visivi per capire le relazioni tra i vari punti del dataset
  pairs(cbind(data$CONSUMPTION,data[,i]), main = colnames(data)[i])
  bagplot.pairs(cbind(data$CONSUMPTION,data[,i]), main = colnames(data)[i])
  ## voglio capire se ci siano variabili per cui una relazione lineare è sufficiente o è necessario modellare nonlinearmente tutte le covariate
  with(data, scatterplotMatrix(data.frame(data$CONSUMPTION, data[,i])))
  ## variabili che proviamo a trattare con modello lineare: LENGTH, POWER, BATTERY_CAPACITY
}

scaled_data <- data
scaled_data[,1:12] <- scale(data[,1:12])

gam_CONSUMPTION14 <- gam(CONSUMPTION ~ s(HEIGHT, bs="cr") + s(PRICE, bs="cr"), data=scaled_data)

gam_CONSUMPTION15 <- gam(CONSUMPTION ~ s(HEIGHT, bs="cr") + s(LENGTH, bs="cr"), data=scaled_data)

pred_gam_CONSUMPTION15=predict(gam_CONSUMPTION15,newdata=grid_HEIGHT_LENGTH)
persp3d(HEIGHT.grid,LENGTH.grid,pred_gam_CONSUMPTION15,col='grey30')
points3d(scaled_data$HEIGHT,scaled_data$LENGTH,scaled_data$CONSUMPTION,col=clustering.m3,size=5)
#points3d(scaled_data$HEIGHT,scaled_data$LENGTH,scaled_data$CONSUMPTION,col=clustering.m2,size=5)
#points3d(scaled_data$HEIGHT,scaled_data$LENGTH,scaled_data$CONSUMPTION,col=clustering.w3,size=5)

###########################################################################################

for (i in 2:12){
  ##check visivi per capire le relazioni tra i vari punti del dataset
  pairs(cbind(data$RANGE,data[,i]), main = colnames(data)[i])
  bagplot.pairs(cbind(data$RANGE,data[,i]), main = colnames(data)[i])
  ## voglio capire se ci siano variabili per cui una relazione lineare è sufficiente o è necessario modellare nonlinearmente tutte le covariate
  with(data, scatterplotMatrix(data.frame(data$RANGE, data[,i])))
  ## variabili che proviamo a trattare con modello lineare: ACC, LENGTH, POWER, BATTERY_CAPACITY (perfetta)
}

gam_RANGE_1 <- gam(RANGE ~ s(HEIGHT, bs="cr") + s(LENGTH, bs="cr"), data=scaled_data)

gam_RANGE_2 <- gam(RANGE~ s(POWER, bs="cr") + s(LENGTH, bs="cr"), data=scaled_data)

gam_RANGE_3 <- gam(RANGE ~ PAYLOAD + s(LENGTH, bs="cr"), data=scaled_data)

pred_gam_RANGE_1=predict(gam_RANGE_1,newdata=grid_HEIGHT_LENGTH)
persp3d(HEIGHT.grid,LENGTH.grid,pred_gam_RANGE_1,col='grey30')
points3d(scaled_data$HEIGHT,scaled_data$LENGTH,scaled_data$RANGE,col=clustering.m3,size=5)
#points3d(scaled_data$HEIGHT,scaled_data$LENGTH,scaled_data$RANGE,col=clustering.m2,size=5)
#points3d(scaled_data$HEIGHT,scaled_data$LENGTH,scaled_data$RANGE,col=clustering.w3,size=5)

################################################################################################

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

##########################################################################################

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
