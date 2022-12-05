library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(mgcv)
library(dplyr)
library(DepthProc)
library(aplpack)
library(cluster)

B = 10000
seed = 26111992

# setwd("C:/Users/UTENTE/Downloads/Nonparametric Statistics/Project/Electric_Vehicles")

Vehicles$Seats <- as.factor(Vehicles$Seats)
Vehicles$Charge.Power <- as.factor(Vehicles$Charge.Power)
Vehicles$Drive <- as.factor(Vehicles$Drive)
Vehicles$Available <- as.factor(Vehicles$Available)

factors <- data.frame(Seats = Vehicles$Seats, Charge.Power = Vehicles$Charge.Power, Drive = Vehicles$Drive)
## elimino covariate non necessarie per la regressione (da analisi correlazione e ooutlier)
data <- as.data.frame(cbind(ACC = Vehicles$Acceleration.0...100.km.h, LENGTH = Vehicles$Length, HEIGHT = Vehicles$Height, PAYLOAD = Vehicles$Max..Payload, CARGO_VOL = Vehicles$Cargo.Volume,
                            RANGE = Vehicles$Electric.Range, POWER = Vehicles$Total.Power,
                            CHARGE_SPEED = Vehicles$Charge.Speed, BATTERY_CAPACITY = Vehicles$Battery.Capacity, FASTCHARGE_SPEED = Vehicles$Fastcharge.Speed,
                            PRICE = Vehicles$Price, CONSUMPTION = Vehicles$Consumption))
# standardize data
data=as.data.frame(scale(data))

# add categorical variables
data <- data %>% mutate(factors)

data <- na.omit(data)

data.e <- daisy(data,metric = "gower")
data.em <- hclust(data.e, method = "mcquitty")
data.ew <- hclust(data.e, method = "ward.D")
clustering.m3 <- as.factor(cutree(data.em,k=3)-1)
clustering.w3 <- as.factor(cutree(data.ew,k=3)-1)
clustering.m2 <- as.factor(cutree(data.em,k=2)-1)
## INSERISCO IL CLUSTERING COME COVARIATA:  in questo caso bisognerebbe trovare un modo per applicare la relazione della dummy variable rispetto a certe covariate come l'altezza (mixed effects models)
data.m2 <- data %>% mutate(clustering.m2)
data.m3 <- data %>% mutate(clustering.m3)
data.w3 <- data %>% mutate(clustering.w3)

for (i in 1:12){
  ##check visivi per capire le relazioni tra i vari punti del dataset
  pairs(cbind(data$CONSUMPTION,data[,i]), main = colnames(data)[i])
  bagplot.pairs(cbind(data$CONSUMPTION,data[,i]), main = colnames(data)[i])
  ## voglio capire se ci siano variabili per cui una relazione lineare è sufficiente o è necessario modellare nonlinearmente tutte le covariate
  with(data, scatterplotMatrix(data.frame(data$CONSUMPTION, data[,i])))
  ## variabili che proviamo a trattare con modello lineare: LENGTH, POWER, BATTERY_CAPACITY
}

##### QUALCHE COMMENTO ALLA FINE
## primo modello: includo tutte le covariate
gam_model <- gam(CONSUMPTION ~ s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                 + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(PRICE,bs="cr")
                 + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Seats + Drive + Charge.Power + clustering.m2, data=data.m2)
summary(gam_model)
# la categorica charge.power, range, charge_speed e battery_capacity super significative
# anche payload molto significativa
# clustering sembra rilevante

## secondo modello: escludo seats,length, acceleration e cargo_vol
gam_model2.m2 <- gam(CONSUMPTION ~ s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr")
                     + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(PRICE,bs="cr")
                     + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Drive + Charge.Power + clustering.m2, data = data.m2)
summary(gam_model2.m2)
# clustering sembra essere ancora più rilevante
# assumono grande significatività anche payload e price
# fastcharge_speed sembra più rilevante

gam_model2.m3 <- gam(CONSUMPTION ~ s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr")
                     + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(PRICE,bs="cr")
                     + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Drive + Charge.Power + clustering.m3, data = data.m3)
summary(gam_model2.m3)
# clustering sembra leggermente meno rilevante rispetto al caso m2
# qui height perde rilevanza

gam_model2.w3 <- gam(CONSUMPTION ~ s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr")
                     + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(PRICE,bs="cr")
                     + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Drive + Charge.Power + clustering.w3, data = data.w3)
summary(gam_model2.w3)
# clustering sembra più rilevante
# qui height e power sembrano avere più significatività

##### Provo a migliorare questi modelli separatamente

### model.m2
# tolgo power
gam_model2.m2 <- gam(CONSUMPTION ~ s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr")
                     + s(RANGE,bs="cr") + s(PRICE,bs="cr")
                     + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Drive + Charge.Power + clustering.m2, data = data.m2)
summary(gam_model2.m2)

# tolgo height
gam_model2.m2 <- gam(CONSUMPTION ~ s(PAYLOAD,bs="cr")
                     + s(RANGE,bs="cr") + s(PRICE,bs="cr")
                     + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Drive + Charge.Power + clustering.m2, data = data.m2)
summary(gam_model2.m2)

# tolgo drive
gam_model2.m2 <- gam(CONSUMPTION ~ s(PAYLOAD,bs="cr") + s(RANGE,bs="cr") + s(PRICE,bs="cr")
                     + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Charge.Power + clustering.m2, data = data.m2)
summary(gam_model2.m2)
# così tutte le variabili sembrano molto significative
# controllare collinearità?

### model.m3
# tolgo height
gam_model2.m3 <- gam(CONSUMPTION ~ s(PAYLOAD,bs="cr")
                     + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(PRICE,bs="cr")
                     + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Drive + Charge.Power + clustering.m3, data = data.m3)
summary(gam_model2.m3)

# tolgo power
gam_model2.m3 <- gam(CONSUMPTION ~ s(PAYLOAD,bs="cr") + s(RANGE,bs="cr") + s(PRICE,bs="cr")
                     + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Drive + Charge.Power + clustering.m3, data = data.m3)
summary(gam_model2.m3)
# così tutte le variabili sembrano molto significative
# controllare collinearità?

### model.w3
# tolgo solo height
gam_model2.w3 <- gam(CONSUMPTION ~ s(PAYLOAD,bs="cr")
                     + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(PRICE,bs="cr")
                     + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Drive + Charge.Power + clustering.w3, data = data.w3)
summary(gam_model2.w3)

# tolgo solo power(quindi rimetto height)
gam_model2.w3 <- gam(CONSUMPTION ~ s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr")
                     + s(RANGE,bs="cr") + s(PRICE,bs="cr")
                     + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Drive + Charge.Power + clustering.w3, data = data.w3)
summary(gam_model2.w3)

# tolgo height e power
gam_model2.w3 <- gam(CONSUMPTION ~ s(PAYLOAD,bs="cr") + s(RANGE,bs="cr") + s(PRICE,bs="cr")
                     + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Drive + Charge.Power + clustering.w3, data = data.w3)
summary(gam_model2.w3)
# così tutte le variabili sembrano molto significative
# controllare collinearità?

##### COMMENTI
# Dopo aver eliminato le variabili che sembrano essere veramente poco rilevanti,
# ovvero seats,length, acceleration e cargo_vol,
# nei vari modelli height e power sembrano essere poco significative rispetto alle altre
# Inoltre sia nel ward che nel mcquitty un terzo cluster sembra esseere di troppo

plot(gam_model2.m2) # senza drive
plot(gam_model2.m3)
plot(gam_model2.w3)
