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
path <- "/Users/Lorenzo/Desktop/Nonpar/Electric_Vehicles/evdatawithprices.csv"
Vehicles <- read.csv(path,sep = ";")

Vehicles$Seats <- as.factor(Vehicles$Seats)
Vehicles$Charge.Power <- as.factor(Vehicles$Charge.Power)
Vehicles$Drive <- as.factor(Vehicles$Drive)
Vehicles$Available <- as.factor(Vehicles$Available)

factors <- data.frame(Seats = Vehicles$Seats, Charge.Power = Vehicles$Charge.Power, Drive = Vehicles$Drive)
## elimino covariate non necessarie per la regressione (da analisi correlazione e ooutlier)
data <- as.data.frame(cbind(RANGE = Vehicles$Electric.Range, ACC = Vehicles$Acceleration.0...100.km.h, LENGTH = Vehicles$Length, HEIGHT = Vehicles$Height, 
                            PAYLOAD = Vehicles$Max..Payload, CARGO_VOL = Vehicles$Cargo.Volume, POWER = Vehicles$Total.Power,
                            CHARGE_SPEED = Vehicles$Charge.Speed, BATTERY_CAPACITY = Vehicles$Battery.Capacity, FASTCHARGE_SPEED = Vehicles$Fastcharge.Speed,
                            PRICE = Vehicles$Price, CONSUMPTION = Vehicles$Consumption))
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

for (i in 2:12){
  ##check visivi per capire le relazioni tra i vari punti del dataset
  pairs(cbind(data$RANGE,data[,i]), main = colnames(data)[i])
  bagplot.pairs(cbind(data$RANGE,data[,i]), main = colnames(data)[i])
  ## voglio capire se ci siano variabili per cui una relazione lineare è sufficiente o è necessario modellare nonlinearmente tutte le covariate
  with(data, scatterplotMatrix(data.frame(data$RANGE, data[,i])))
  ## variabili che proviamo a trattare con modello lineare: ACC, LENGTH, POWER, BATTERY_CAPACITY (perfetta)
}

## ATTENZIONE: i punti outlier o comunque tutti i punti che rimangono ai margini della distribuzione 
# hanno un effetto molto importante: come trattiamo questa situazione?
# In particolare capita con 
## variabili che presentano questo tipo di problemi: HEIGHT, PAYLOAD, CARGO_VOL, CHARGE_SPEED, CONSUMPTION

## FASTCHARGE SPEED è come se avesse due cluster con due rette differenti, 
#anche se nello stesso intervallo di RANGE

## HEIGHT cambia un botto, come vediamo dai grafici sotto

data2 <- data[-c(which(data$HEIGHT>1800)),]
with(data, scatterplotMatrix(data.frame(data$RANGE, data$HEIGHT)))
fit_full<-lm(RANGE~HEIGHT, data)
summary(fit_full)
with(data2, scatterplotMatrix(data.frame(data2$RANGE, data2$HEIGHT)))
fit_part<-lm(RANGE~HEIGHT, data2)
summary(fit_part)

## primo modello: includo tutte le covariate
gam_model <- gam(RANGE ~ s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                 + s(PRICE,bs="cr") + s(POWER,bs="cr") + s(CONSUMPTION,bs="cr")
                 + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Seats + Drive + Charge.Power, data=data.m2)
summary(gam_model)

#Drive come categorica non sembra significativa, POWER come continua nemmeno; escludiamo
## secondo modello: tengo in considerazione il clustering con le variabili selezionate prima
gam_model2.m2 <- gam(RANGE ~ s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                     + s(PRICE,bs="cr") + s(CONSUMPTION,bs="cr") + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") 
                     + s(FASTCHARGE_SPEED,bs="cr")  + Seats + Charge.Power + clustering.m2, data = data.m2)
summary(gam_model2.m2)

## Non significativo il clustering qui, proviamo con il secondo

gam_model2.m3 <- gam(RANGE ~ s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                     + s(PRICE,bs="cr") + s(CONSUMPTION,bs="cr") + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") 
                     + s(FASTCHARGE_SPEED,bs="cr")  + Seats + Charge.Power + clustering.m3, data = data.m3)
summary(gam_model2.m3)

# Nemmeno questo è valido, proviamo l'ultimo

gam_model2.w3 <- gam(RANGE ~ s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                     + s(PRICE,bs="cr") + s(CONSUMPTION,bs="cr") + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") 
                     + s(FASTCHARGE_SPEED,bs="cr")  + Seats + Charge.Power + clustering.w3, data = data.w3)
summary(gam_model2.w3)

#Neanche questo qua;

#A sto punto i cluster li escludo, direi non mi servono per la regressione di RANGE

gam_model <- gam(RANGE ~ s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                 + s(PRICE,bs="cr")  + s(CONSUMPTION,bs="cr") + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") 
                 + s(FASTCHARGE_SPEED,bs="cr") + Seats + Charge.Power, data=data.m2)
summary(gam_model)

# Al pelo, ma alla fine dei conti eliminerei anche Charge.Power (categorica) e ACC, 
# così da rendere il tutto più snello e semplificare, che è sempre un bene

gam_model <- gam(RANGE ~ s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                 + s(PRICE,bs="cr")  + s(CONSUMPTION,bs="cr") + s(CHARGE_SPEED,bs="cr") 
                 + s(BATTERY_CAPACITY,bs="cr")+ s(FASTCHARGE_SPEED,bs="cr") + Seats, data=data.m2)
summary(gam_model)

#### CAMBIO APPROCCIO
gam_RANGE <- vector(mode = "list", length = 12)
for (i in 2:12){
  gam_RANGE[[i]] <- gam(RANGE ~ s(data[,i], bs="cr"), data=data)
  print(summary(gam_RANGE[[i]])[10])
}
#SAREBBERO BATTERY CAPACITY E CHARGE SPEED; ma vanno bene?
# secondo me meglio POWER e ACC, più bassi ma confrontabili con il non elettrico
gam_RANGED <- vector(mode = "list", length = 12)
for (i in 2:12){
  gam_RANGED[[i]] <- gam(RANGE ~ s(data[,i], bs="cr") + Drive, data=data)
  print(summary(gam_RANGED[[i]])[10]) 
}
#Drive alza abbbastanza: tengo sempre POWER e ACC come i migliori risultati, Drive sembra 
# una buona categorica e alza soprattutto le fisiche (0.3->0.5)
gam_RANGEC1 <- vector(mode = "list", length = 12)
for (i in 2:12){
  gam_RANGEC1[[i]] <- gam(RANGE ~ s(data[,i], bs="cr") + as.factor(clustering.m2), data=data)
  print(summary(gam_RANGEC1[[i]])[10]) 
}
# migliora il risultato, ma drive mi pare ancora meglio
gam_RANGEC2 <- vector(mode = "list", length = 12)
for (i in 1:12){
  gam_RANGEC2[[i]] <- gam(PRICE ~ s(data[,i], bs="cr") + clustering.m3, data=data)
  print(summary(gam_RANGEC2[[i]])[10]) 
}
# questo brutto brutto
gam_RANGEC3 <- vector(mode = "list", length = 12)
for (i in 1:12){
  gam_RANGEC3[[i]] <- gam(PRICE ~ s(data[,i], bs="cr") + clustering.w3, data=data)
  print(summary(gam_RANGEC3[[i]])[10]) 
}
# CL1 o CL3 i migliori a disposizione


## SMOOTHING SPLINES CON POTENZA COME REGRESSORE
fit_smooth_spline_CV <- with(data, smooth.spline(x = POWER, y = RANGE,cv = TRUE)) 
fit_smooth_spline_GCV <- with(data, smooth.spline(x = POWER, y = RANGE,cv = FALSE)) 
with(data, plot(POWER, RANGE, cex =.5, col =" darkgrey ")) 
lines(fit_smooth_spline_CV,col="red",lwd=2,lty=1) 
lines(fit_smooth_spline_GCV,col="blue",lwd=2, lty=2) 

## SMOOTHING SPLINES CON ACC COME REGRESSORE
fit_smooth_spline_CV <- with(data, smooth.spline(x = ACC, y = RANGE,cv = TRUE)) 
fit_smooth_spline_GCV <- with(data, smooth.spline(x = ACC, y = RANGE,cv = FALSE)) 
with(data, plot(ACC, RANGE, cex =.5, col =" darkgrey ")) 
lines(fit_smooth_spline_CV,col="red",lwd=2,lty=1) 
lines(fit_smooth_spline_GCV,col="blue",lwd=2, lty=2) 

## FITTIAMO ALCUNI MODELLI CHE POSSONO ESSERE MOLTO MOLTO BUONI
scaled_data <- data
scaled_data[,1:12] <- scale(data[,1:12])
## MODELLO 1: 0.69 buono
gam_RANGE1 <- gam(RANGE ~ s(POWER, bs="cr") + s(ACC, bs="cr") + s(I(POWER*ACC),bs="cr") + clustering.m2, data=scaled_data)
summary(gam_RANGE1)
## MODELLO 2(senza interaction): 0.67, posso tenere questo a sto punto
gam_RANGE2 <- gam(RANGE ~ s(POWER, bs="cr") + s(ACC, bs="cr") + clustering.m2, data=scaled_data)
summary(gam_RANGE2)
## MODELLO 3 senza cluster: 0.67
gam_RANGE3 <- gam(RANGE ~ s(POWER, bs="cr") + s(ACC, bs="cr") + s(I(POWER*ACC),bs="cr"), data=scaled_data)
summary(gam_RANGE3)
## MODELLO 4: cluster 3 0.69
gam_RANGE4 <- gam(RANGE ~ s(POWER, bs="cr") + s(ACC, bs="cr") + s(I(POWER*ACC),bs="cr") + clustering.w3, data=scaled_data)
summary(gam_RANGE4)
## MODELLO 5: con Drive 0.69
gam_RANGE1 <- gam(RANGE ~ s(POWER, bs="cr") + s(ACC, bs="cr") + s(I(POWER*ACC),bs="cr") + Drive, data=scaled_data)
summary(gam_RANGE1)

### PASSO A CARATTERISTICHE FISICHE: MOLTO MEGLIO! (anche le variabili le preferisco)
#Modello 1: tutto significativo, 0.78
gam_RANGE_1 <- gam(RANGE ~ s(HEIGHT, bs="cr") + s(LENGTH, bs="cr") + s(I(HEIGHT*LENGTH),bs="cr") + clustering.m2, data=scaled_data)
summary(gam_RANGE_1)
#Modello 2: 0.76
gam_RANGE_2 <- gam(RANGE~ s(POWER, bs="cr") + s(LENGTH, bs="cr") + s(I(POWER*LENGTH),bs="cr") + clustering.m2, data=scaled_data)
summary(gam_RANGE_2)
#MODELLO MIGLIORE, 0.8 teniamo questo, ci piace lui per le variabili, il payload per il range
gam_RANGE_3 <- gam(RANGE ~ PAYLOAD + s(LENGTH, bs="cr") + s(I(PAYLOAD*LENGTH),bs="cr") + clustering.m3, data=scaled_data)
summary(gam_RANGE_3)

