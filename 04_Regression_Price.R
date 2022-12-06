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
path <- "/Users/luca/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Nonparam Project/DATASET/evdatawithprices.csv"
Vehicles <- read.csv(path,sep = ";")

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
data <- data %>% mutate(factors)

data <- na.omit(data)
data[,1:12] <- scale(data[,1:12])
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
  pairs(cbind(data$PRICE,data[,i]), main = colnames(data)[i])
  bagplot.pairs(cbind(data$PRICE,data[,i]), main = colnames(data)[i])
  ## voglio capire se ci siano variabili per cui una relazione lineare è sufficiente o è necessario modellare nonlinearmente tutte le covariate
  with(data, scatterplotMatrix(data.frame(data$PRICE, data[,i])))
  ## variabili che proviamo a trattare con modello lineare: FASTCHARGE_SPEED, POWER, RANGE, LENGTH
}

## ATTENZIONE: i punti outlier o comunque tutti i punti che rimangono ai margini della distribuzione hanno un effetto molto importante: come trattiamo questa situazione?
## variabili che presentano questo tipo di problemi: LENGTH, MAX_PAYLOAD, CARGO_VOL, CHARGE_SPEED
# data2 <- data[-c(which(data$CHARGE_SPEED %in% boxplot.stats(data$CHARGE_SPEED)$out)),]
# with(data2, scatterplotMatrix(data.frame(data2$PRICE, data2$CHARGE_SPEED)))
# da qui possiamo osservare quanto siano influenti quei punti alla fine del dominio!! (domanda per cappozzo)

## primo modello: includo tutte le covariate
gam_model <- gam(PRICE ~ s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                 + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(CONSUMPTION,bs="cr")
                 + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Seats + Drive + Charge.Power, data=data.m2)
summary(gam_model)

## secondo modello: escludo le categoriche (tengo in considerazione il clustering) e due continue
gam_model2.m2 <- gam(PRICE ~  s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                     + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(CONSUMPTION,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + clustering.m2, data = data.m2)
summary(gam_model2.m2)

gam_model2.m3 <- gam(PRICE ~  s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                  + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(CONSUMPTION,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + clustering.m3, data = data.m3)
summary(gam_model2.m3)



## unico clustering che esce decente ma devo raggruppare due clustering:
gam_model2.w3 <- gam(PRICE ~  s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                  + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + clustering.w3, data = data.w3)
summary(gam_model2.w3)

## terzo modello: escludo le categoriche tranne la TRAZIONE
gam_model.drive <- gam(PRICE ~  s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                     + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + Drive, data = data.m3)
summary(gam_model.drive)

gam_model2.drive <- gam(PRICE ~  s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") +
                        + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + Drive + CARGO_VOL, data = data.m3)
summary(gam_model2.drive)

plot(gam_model2.drive)

## da rivedere questo confronto ANOVA (1: non so se le assunzioni vengano rispettate, 2: vale davvero la pena tornare al modello più complesso)
anova(gam_model2.drive,gam_model.drive,test="F")

## proviamo a fittare con natural cubic splices
#model_gam_ns <-   lm(prestige ~ ns(education, df = 3) + ns(income, df = 3), data = Prestige
                     
                     