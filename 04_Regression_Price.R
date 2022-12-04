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

data.e <- daisy(data,metric = "gower")
data.em <- hclust(data.e, method = "mcquitty")
clustering <- cutree(data.em,k=2)-1
## INSERISCO IL CLUSTERING COME COVARIATA:  in questo caso bisognerebbe trovare un modo per applicare la relazione della dummy variable rispetto a certe covariate come l'altezza (mixed effects models)
data <- data %>% mutate(clustering)

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
data2 <- data[-c(which(data$CHARGE_SPEED %in% boxplot.stats(data$CHARGE_SPEED)$out)),]
with(data2, scatterplotMatrix(data.frame(data2$PRICE, data2$CHARGE_SPEED)))
# da qui possiamo osservare quanto siano influenti quei punti alla fine del dominio!! (domanda per cappozzo)

## primo modello: includo tutte le covariate
gam_model <- gam(PRICE ~ s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                 + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(CONSUMPTION,bs="cr")
                 + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Seats + Drive + Charge.Power + clustering, data=data)
summary(gam_model)

## secondo modello: escludo le categoriche (tranne la trazione)
gam_model2 <- gam(PRICE ~  s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                  + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + Drive, data = data)

summary(gam_model2)

gam_model2 <- gam(PRICE ~  s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
                  + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(BATTERY_CAPACITY,bs="cr"), data = data)

summary(gam_model2)

## da qui ci accorgiamo che i punti outlier o che comunque si trovano alla fine del dominio vanno trattati in maniera differente
# gam_model2 <- gam(PRICE ~ s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
#                  + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(CONSUMPTION,bs="cr")
#                  + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr"), data=data2)
# 
# summary(gam_model2)


# # simple linear regression
# model_lm1=lm(Price ~ Electric.Range 
#                   + Vehicle.Fuel.Euivalent 
#                   + Charge.Speed 
#                   + Total.Power
#                   + Height
#                   + Length
#                   + Max..Payload
#                   + Cargo.Volume, data=data)
# summary(model_lm1)
# 
# # tolgo charge.speed
# model_lm2=lm(Price ~ Electric.Range 
#              + Vehicle.Fuel.Euivalent 
#              + Total.Power
#              + Height
#              + Length
#              + Max..Payload
#              + Cargo.Volume, data=data)
# summary(model_lm2)
# 
# # tolgo max.payload
# model_lm3=lm(Price ~ Electric.Range 
#              + Vehicle.Fuel.Euivalent 
#              + Total.Power
#              + Height
#              + Length
#              + Cargo.Volume, data=data)
# summary(model_lm3)
# 
# # tolgo electric.range
# model_lm4=lm(Price ~ Vehicle.Fuel.Euivalent 
#              + Total.Power
#              + Height
#              + Length
#              + Cargo.Volume, data=data)
# summary(model_lm4)
# 
# # per ora sembra essere il miglior modello (linear regression)
# # ma le assunzioni per regressione lineare non sono soddisfatte (come ci aspettavamo)
# plot(model_lm4)
# shapiro.test(residuals(model_lm4))
# 
# #GLM
# # (1)cubic spline basis
# model_gam1=gam(Price ~ s(Electric.Range,bs='cr') 
#                     + s(Vehicle.Fuel.Euivalent,bs='cr')
#                     + s(Charge.Speed,bs='cr') 
#                     + s(Total.Power,bs='cr') 
#                     + s(Height,bs='cr') 
#                     + s(Length,bs='cr') 
#                     + s(Max..Payload,bs='cr') 
#                     + s(Cargo.Volume,bs='cr'), data=data
#               )
# summary(model_gam1)
# # alto R-sq 0.971, ma max.payload sembra poco significativo
# 
# # tolgo max.paypload
# model_gam2=gam(Price ~ s(Electric.Range,bs='cr') 
#                + s(Vehicle.Fuel.Euivalent,bs='cr')
#                + s(Charge.Speed,bs='cr') 
#                + s(Total.Power,bs='cr') 
#                + s(Height,bs='cr') 
#                + s(Length,bs='cr') 
#                + s(Cargo.Volume,bs='cr'), data=data
#               )
# summary(model_gam2)
# # quasi identico R-sq 0.969, tutte variabili significative
# 
# # residuals
# hist(model_gam2$residuals)
# qqnorm(model_gam2$residuals)
# 
# # normality test
# shapiro.test(model_gam2$residuals)
# 
# # (2)natural cubic splines
# model_gam_ns =
#   lm(Price ~ ns(Electric.Range,df=8) 
#             + ns(Vehicle.Fuel.Euivalent,df=8)
#             + ns(Charge.Speed,df=8) 
#             + ns(Total.Power,df=8) 
#             + ns(Height,df=8) 
#             + ns(Length,df=8) 
#             + ns(Cargo.Volume,df=8), data=data
#      )
# # summary(model_gam_ns)
# 
# # scatterplot for the two models
# plot(model_gam_ns$residuals,model_gam2$residuals)
# cor(model_gam_ns$residuals,model_gam2$residuals)
# 
# # Consider model with cubic spline basis
# # I look at the contribution of a single predictor 
# # holding all the others fixed
# plot(model_gam2)