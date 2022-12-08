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

#setwd("C:/Users/UTENTE/Downloads/Nonparametric Statistics/Project/Electric_Vehicles")

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

##### NOW NEW APPROACH
gam_CONSUMPTION <- vector(mode = "list", length = 12)
for (i in 1:12){
  gam_CONSUMPTION[[i]] <- gam(CONSUMPTION ~ s(data[,i], bs="cr"), data=data)
  print(summary(gam_CONSUMPTION[[i]])[10]) 
  ## HEIGHT dà l'r-squared più alto(0.75), seguito  da charge_speed, acc, payload
}
gam_CONSUMPTION <- vector(mode = "list", length = 12)
for (i in 1:12){
  gam_CONSUMPTION[[i]] <- gam(CONSUMPTION ~ s(data[,i], bs="cr") + Drive, data=data)
  print(summary(gam_CONSUMPTION[[i]])[10])
  ## così tutte aumentano la significatività
  ## HEIGHT dà l'r-squared più alto, seguito da payload, charge_speed, acc
}
gam_CONSUMPTION <- vector(mode = "list", length = 12)
for (i in 1:12){
  gam_CONSUMPTION[[i]] <- gam(CONSUMPTION ~ s(data[,i], bs="cr") + clustering.m2, data=data)
  print(summary(gam_CONSUMPTION[[i]])[10])
  ## adesso tutte aumentate molto!
  ## CHARGE_SPEED dà l'r-squared più alto, seguito da price, battery, length
}
gam_CONSUMPTION <- vector(mode = "list", length = 12)
for (i in 1:12){
  gam_CONSUMPTION[[i]] <- gam(CONSUMPTION ~ s(data[,i], bs="cr") + clustering.m3, data=data)
  print(summary(gam_CONSUMPTION[[i]])[10])
  ## anche qui tutte aumentate molto!
  ## CHARGE_SPEED dà l'r-squared più alto, seguito da price, battery, height
}
gam_CONSUMPTION <- vector(mode = "list", length = 12)
for (i in 1:12){
  gam_CONSUMPTION[[i]] <- gam(CONSUMPTION ~ s(data[,i], bs="cr") + clustering.w3, data=data)
  print(summary(gam_CONSUMPTION[[i]])[10])
  ## anche qui tutte aumentate molto! Insomma clustering si rivela importante
  ## CHARGE_SPEED e HEIGHT hanno l'r-squared più alto(>0.8), seguiti da price, range
}

## SMOOTHING SPLINES CON ALTEZZA COME REGRESSORE
fit_smooth_spline_CV <- with(data, smooth.spline(x = HEIGHT, y = CONSUMPTION,cv = TRUE)) 
fit_smooth_spline_GCV <- with(data, smooth.spline(x = HEIGHT, y = CONSUMPTION,cv = FALSE)) 
with(data, plot(HEIGHT, CONSUMPTION, cex =.5, col =" darkgrey ")) 
lines(fit_smooth_spline_CV,col="red",lwd=2,lty=1) 
lines(fit_smooth_spline_GCV,col="blue",lwd=2, lty=2) 
legend(400, 50000, legend=c("CV", "GCV"), col=c("red", "blue"), lty=1:2, cex=0.8)

gam_CONSUMPTION <- gam(CONSUMPTION ~ s(HEIGHT, bs="cr"), data=data)

## CUBIC SPLINES CON ALTEZZA COME REGRESSORE
new_data <- with(data, data.frame(HEIGHT = seq(range(HEIGHT)[1], range(HEIGHT)[2], by = 0.5)))
preds <- predict(gam_CONSUMPTION, new_data,se=T) 
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
with(data, plot(HEIGHT ,CONSUMPTION ,xlim=range(new_data$HEIGHT),cex =.5, col = clustering.m2)) 
lines(new_data$HEIGHT,preds$fit ,lwd =2, col =" blue")
matlines(new_data$HEIGHT, se.bands ,lwd =1, col =" blue",lty =3)

## NATURAL CUBIC SPLINES CON ALTEZZA COME REGRESSIORE
knots <- quantile(data$HEIGHT,probs=c(0.1,0.3,0.5,0.7,0.9)) 
boundary_knots <- quantile(data$HEIGHT,probs=c(0.05,0.95))

## NON SO PERCHE' MA MI DA' ERRORE
model_ns <- lm(CONSUMPTION ~ ns(HEIGHT,knots=knots,Boundary.knots=boundary_knots), data=data) #defaults to three kn ots
preds <- predict(model_ns, new_data,se=T) 
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
with(data, plot(HEIGHT ,CONSUMPTION ,xlim=range(new_data$HEIGHT) ,cex =.5, col =" darkgrey " )) 
lines(new_data$HEIGHT,preds$fit ,lwd =2, col =" blue")
matlines(new_data$HEIGHT, se.bands ,lwd =1, col =" blue",lty =3)
knots_pred=predict(model_ns,list(HEIGHT=knots)) 
points(knots,knots_pred, col='blue',pch=19) 
boundary_pred <- predict(model_ns,list(HEIGHT=boundary_knots))
points(boundary_knots,boundary_pred,col='red',pch=19) 
abline(v = knots, lty=3, col="blue") 
abline(v = boundary_knots, lty=3, col="red")

## LET'S FIT SOME GOOD MODELS
scaled_data <- data
scaled_data[,1:12] <- scale(data[,1:12])
## MODELLO 1: SENZA CATEGORICGHE GIA' SOPRA 0.8
gam_CONSUMPTION1 <- gam(CONSUMPTION ~ s(HEIGHT, bs="cr") + s(CHARGE_SPEED, bs="cr"), data=scaled_data)
summary(gam_CONSUMPTION1)
## MODELLO 2: CON INTERAZIONE MIGLIORA LEGGERMENTE
gam_CONSUMPTION2 <- gam(CONSUMPTION ~ s(HEIGHT, bs="cr") + s(CHARGE_SPEED, bs="cr") + s(I(HEIGHT*CHARGE_SPEED),bs="cr"), data=scaled_data)
summary(gam_CONSUMPTION2)
## MODELLO 3: modello 1 + Drive MIGLIORA LEGGERMENTE (similie al modello 2)
gam_CONSUMPTION3 <- gam(CONSUMPTION ~ s(HEIGHT, bs="cr") + s(CHARGE_SPEED, bs="cr") + Drive, data=scaled_data)
summary(gam_CONSUMPTION3)
## MODELLO 4: Drive e INTERAZIONE INSIEME NON VANNO A MIGLIORARE
gam_CONSUMPTION4 <- gam(CONSUMPTION ~ s(HEIGHT, bs="cr") + s(CHARGE_SPEED, bs="cr") + s(I(HEIGHT*CHARGE_SPEED),bs="cr") + Drive, data=scaled_data)
summary(gam_CONSUMPTION4)
## MODELLO 5: MIGLIORA LEGGERMENTE
gam_CONSUMPTION5 <- gam(CONSUMPTION ~ s(HEIGHT, bs="cr") + s(CHARGE_SPEED, bs="cr") + s(I(HEIGHT*CHARGE_SPEED),bs="cr") + clustering.w3, data=scaled_data)
summary(gam_CONSUMPTION5)
## MODELLO 6: MOLTO BENE
gam_CONSUMPTION6 <- gam(CONSUMPTION ~ s(HEIGHT, bs="cr") + s(CHARGE_SPEED, bs="cr") + s(PRICE, bs="cr") + s(I(HEIGHT*CHARGE_SPEED),bs="cr") + clustering.w3, data=scaled_data)
summary(gam_CONSUMPTION6)
## MODELLO 7: BENE 
gam_CONSUMPTION7 <- gam(CONSUMPTION ~ s(CHARGE_SPEED, bs="cr") + s(PRICE, bs="cr") + clustering.m2, data=scaled_data)
summary(gam_CONSUMPTION7)
## MODELLO 8: L'INTERAZIONE CHARGE_SPEED/PRICE RISULA MOLTO SIGNIFICATIVA!
gam_CONSUMPTION8 <- gam(CONSUMPTION ~ s(CHARGE_SPEED, bs="cr") + s(PRICE, bs="cr") + s(I(CHARGE_SPEED*PRICE),bs="cr") + clustering.m2, data=scaled_data)
summary(gam_CONSUMPTION8)
## MODELLO 9: OTTIMO!
gam_CONSUMPTION9 <- gam(CONSUMPTION ~ s(HEIGHT, bs="cr") + s(CHARGE_SPEED, bs="cr") + s(PRICE, bs="cr") + s(I(CHARGE_SPEED*PRICE),bs="cr") + clustering.w3, data=scaled_data)
summary(gam_CONSUMPTION9)
## MODELLO 10: OTTIMO!
gam_CONSUMPTION10 <- gam(CONSUMPTION ~ s(CHARGE_SPEED, bs="cr") + s(PRICE, bs="cr") + s(I(CHARGE_SPEED*PRICE),bs="cr") + clustering.w3, data=scaled_data)
summary(gam_CONSUMPTION10)
## MODELLO 11: SENZA CLUSTERING COMUNQUE ARRIVIAMO A 0.9 
gam_CONSUMPTION11 <- gam(CONSUMPTION ~ s(CHARGE_SPEED, bs="cr") + s(PRICE, bs="cr") + s(I(CHARGE_SPEED*PRICE),bs="cr"), data=scaled_data)
summary(gam_CONSUMPTION11)
## MODELLO 12: CLUSTERING M.3 SIMILE A W.3
gam_CONSUMPTION12 <- gam(CONSUMPTION ~ s(CHARGE_SPEED, bs="cr") + s(PRICE, bs="cr") + s(I(CHARGE_SPEED*PRICE),bs="cr") + clustering.m3, data=scaled_data)
summary(gam_CONSUMPTION12)
## MODELLO 13: CLIUSTERING M.2 SIMILE AGLI ALTRI
gam_CONSUMPTION13 <- gam(CONSUMPTION ~ s(CHARGE_SPEED, bs="cr") + s(PRICE, bs="cr") + s(I(CHARGE_SPEED*PRICE),bs="cr") + clustering.m2, data=scaled_data)
summary(gam_CONSUMPTION13)
## MODELLO 14: SENZA USARE CHARGE_SPEED POSSIAMO RAGGIUNGERE >0.9
##             MIGLIORE CLUSTERING RISULTA W.3 (LEGGERMENTE MEGLIO DEGLI ALTRI)
gam_CONSUMPTION14 <- gam(CONSUMPTION ~ s(HEIGHT, bs="cr") + s(PRICE, bs="cr") + s(I(HEIGHT*PRICE),bs="cr") + clustering.w3, data=scaled_data)
summary(gam_CONSUMPTION14)

##### MIGLIOR MODELLO CON PRICE E CHARGE_SPEED + INTERAZIONE,
##### AGGIUNGENDO CLUSTERING SI PASSA DA 0.9 A CIRCA 0.96
##### MA SUPERIAMO 0.9 ANCHE SENZA CHARGE_SPEED, CON PRICE E HEIGHT