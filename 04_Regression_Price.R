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

## ATTENZIONE: i punti outlier o comunque tutti i punti che rimangono ai margini della distribuzione hanno un effetto molto importante: come trattiamo questa situazione?
## variabili che presentano questo tipo di problemi: LENGTH, MAX_PAYLOAD, CARGO_VOL, CHARGE_SPEED
data2 <- data[-c(which(data$CHARGE_SPEED %in% boxplot.stats(data$CHARGE_SPEED)$out)),]
with(data2, scatterplotMatrix(data.frame(data2$PRICE, data2$CHARGE_SPEED)))
# da qui possiamo osservare quanto siano influenti quei punti alla fine del dominio!! (domanda per cappozzo)
# 
# ## primo modello: includo tutte le covariate
# gam_model <- gam(PRICE ~ s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
#                  + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(CONSUMPTION,bs="cr")
#                  + s(CHARGE_SPEED,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + s(FASTCHARGE_SPEED,bs="cr") + Seats + Drive + Charge.Power, data=data.m2)
# summary(gam_model)
# 
# ## secondo modello: escludo le categoriche (tengo in considerazione il clustering) e due continue
# gam_model2.m2 <- gam(PRICE ~  s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
#                      + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(CONSUMPTION,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + clustering.m2 + Drive, data = data.m2)
# summary(gam_model2.m2)
# 
# gam_model2.m3 <- gam(PRICE ~  s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
#                   + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(CONSUMPTION,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + clustering.m3 + Drive, data = data.m3)
# summary(gam_model2.m3)
# 
# ## unico clustering che esce decente ma devo raggruppare due clustering:
# gam_model2.w3 <- gam(PRICE ~  s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
#                   + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + clustering.w3, data = data.w3)
# summary(gam_model2.w3)
# 
# ## terzo modello: escludo le categoriche tranne la TRAZIONE
# gam_model.drive <- gam(PRICE ~  s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(PAYLOAD,bs="cr") + s(CARGO_VOL,bs="cr")
#                      + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + Drive, data = data.m3)
# summary(gam_model.drive)
# 
# gam_model2.drive <- gam(PRICE ~  s(ACC,bs="cr") + s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") +
#                         + s(RANGE,bs="cr") + s(POWER,bs="cr") + s(BATTERY_CAPACITY,bs="cr") + Drive + CARGO_VOL, data = data.m3)
# summary(gam_model2.drive)
# plot(gam_model2.drive)
# 
# gam_model3 <- gam(PRICE ~  s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(RANGE,bs="cr") + s(BATTERY_CAPACITY,bs="cr") 
#                         + Drive + CARGO_VOL, data = data.m3)
# summary(gam_model3)
# 
# gam_model3 <- gam(PRICE ~  s(LENGTH,bs="cr") + s(HEIGHT,bs="cr") + s(RANGE,bs="cr") + s(BATTERY_CAPACITY,bs="cr") 
#                   + Drive + CARGO_VOL + POWER, data = data)
# summary(gam_model3)


## CAMBIO APPROCCIO: Chiaramente c'è qualcosa che non va nell'approccio seguito in precedenza: proviamo a cambiare approccio
gam_PRICE <- vector(mode = "list", length = 12)
for (i in 1:12){
  gam_PRICE[[i]] <- gam(PRICE ~ s(data[,i], bs="cr"), data=data)
  print(summary(gam_PRICE[[i]])[10]) 
  ## POWER da l'r-squared più alto
}
gam_PRICE <- vector(mode = "list", length = 12)
for (i in 1:12){
  gam_PRICE[[i]] <- gam(PRICE ~ s(data[,i], bs="cr") + Drive, data=data)
  print(summary(gam_PRICE[[i]])[10]) 
  ## POWER da l'r-squared più alto
}
gam_PRICE <- vector(mode = "list", length = 12)
for (i in 1:12){
  gam_PRICE[[i]] <- gam(PRICE ~ s(data[,i], bs="cr") + clustering.m2, data=data)
  print(summary(gam_PRICE[[i]])[10]) 
  ## POWER da l'r-squared più alto
}
gam_PRICE <- vector(mode = "list", length = 12)
for (i in 1:12){
  gam_PRICE[[i]] <- gam(PRICE ~ s(data[,i], bs="cr") + clustering.m3, data=data)
  print(summary(gam_PRICE[[i]])[10]) 
  ## POWER da l'r-squared più alto
}
gam_PRICE <- vector(mode = "list", length = 12)
for (i in 1:12){
  gam_PRICE[[i]] <- gam(PRICE ~ s(data[,i], bs="cr") + clustering.w3, data=data)
  print(summary(gam_PRICE[[i]])[10]) 
  ## POWER da l'r-squared più alto
}

## SMOOTHING SPLINES CON POTENZA COME REGRESSORE
fit_smooth_spline_CV <- with(data, smooth.spline(x = POWER, y = PRICE,cv = TRUE)) 
fit_smooth_spline_GCV <- with(data, smooth.spline(x = POWER, y = PRICE,cv = FALSE)) 
with(data, plot(POWER, PRICE, cex =.5, col =" darkgrey ")) 
lines(fit_smooth_spline_CV,col="red",lwd=2,lty=1) 
lines(fit_smooth_spline_GCV,col="blue",lwd=2, lty=2) 
legend(400, 50000, legend=c("CV", "GCV"), col=c("red", "blue"), lty=1:2, cex=0.8)

gam_PRICE <- gam(PRICE ~ s(POWER, bs="cr"), data=data)

## CUBIC SPLINES CON POTENZA COME REGRESSORE
new_data <- with(data, data.frame(POWER = seq(range(POWER)[1], range(POWER)[2], by = 0.5)))
preds <- predict(gam_PRICE, new_data,se=T) 
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
with(data, plot(POWER ,PRICE ,xlim=range(new_data$POWER),cex =.5, col = clustering.m2)) 
lines(new_data$POWER,preds$fit ,lwd =2, col =" blue")
matlines(new_data$POWER, se.bands ,lwd =1, col =" blue",lty =3)

## NATURAL CUBIC SPLINES CON POTENZA COME REGRESSIORE
knots <- quantile(data$POWER,probs=c(0.1,0.3,0.5,0.7,0.9)) 
boundary_knots <- quantile(data$POWER,probs=c(0.05,0.95))

model_ns <- lm(PRICE ~ ns(POWER,knots=knots,Boundary.knots=boundary_knots), data=data) #defaults to three kn ots
preds <- predict(model_ns, new_data,se=T) 
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
with(data, plot(POWER ,PRICE ,xlim=range(new_data$POWER) ,cex =.5, col =" darkgrey " )) 
lines(new_data$POWER,preds$fit ,lwd =2, col =" blue")
matlines(new_data$POWER, se.bands ,lwd =1, col =" blue",lty =3)
knots_pred=predict(model_ns,list(POWER=knots)) 
points(knots,knots_pred, col='blue',pch=19) 
boundary_pred <- predict(model_ns,list(POWER=boundary_knots))
points(boundary_knots,boundary_pred,col='red',pch=19) 
abline(v = knots, lty=3, col="blue") 
abline(v = boundary_knots, lty=3, col="red")

## FITTIAMO ALCUNI MODELLI CHE POSSONO ESSERE MOLTO MOLTO BUONI
scaled_data <- data
scaled_data[,1:12] <- scale(data[,1:12])
## MODELLO 1: MOLTO BUONO
gam_PRICE1 <- gam(PRICE ~ s(POWER, bs="cr") + s(HEIGHT, bs="cr") + s(I(POWER*HEIGHT),bs="cr") + clustering.m2, data=scaled_data)
summary(gam_PRICE1)
## MODELLO 2: MOLTO BUONO!
gam_PRICE2 <- gam(PRICE ~ s(ACC, bs="cr") + s(HEIGHT, bs="cr") + clustering.m3, data=scaled_data)
summary(gam_PRICE2)
## MODELLO 3: MOLTO BUONO!
gam_PRICE3 <- gam(PRICE ~ s(LENGTH, bs="cr") + s(POWER,bs="cr") + clustering.m2, data=scaled_data)
summary(gam_PRICE3)
## MODELLO 4: MOLTO BUONO
gam_PRICE4 <- gam(PRICE ~ s(LENGTH, bs="cr") + s(POWER,bs="cr") + s(I(LENGTH*POWER),bs="cr"), data=scaled_data)
summary(gam_PRICE4)
## MODELLO 4: MOLTO BUONO!
gam_PRICE5 <- gam(PRICE ~ s(BATTERY_CAPACITY, bs="cr") + s(POWER,bs="cr") + clustering.m2, data=scaled_data)
summary(gam_PRICE5)

# gam_PRICE1 <- gam(PRICE ~ bs(POWER, degree = 3, df = 8) + bs(HEIGHT, degree = 3, df = 7) + s(I(POWER*HEIGHT),bs="cr") + clustering.m2, data=scaled_data)
# summary(gam_PRICE1)
# preds=predict(model_cubic_splines_2, new_data,se=T) 
# se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
# with(data, plot(POWER ,PRICE ,xlim=range(new_data$POWER) ,cex =.5, col =" darkgrey " )) 
# lines(new_data$POWER,preds$fit ,lwd =2, col =" blue")
# matlines(new_data$POWER, se.bands ,lwd =1, col =" blue",lty =3)
# knots <- attr(bs(data$POWER, degree=3,df=7),'knots') 
# knots_pred=predict(model_cubic_splines_2,list(POWER=knots)) 
# points(knots,knots_pred, col='blue',pch=19) 
# abline(v = knots, lty=3)

# ## UTILIZZANDO SOLO ALTEZZA E TRAZIONE RIUSCIAMO AD OTTENERE R-Squared(adj) > 0.8
# gam_simple_CON <- gam(CONSUMPTION ~ s(HEIGHT, bs="cr") + Drive, data = data)
# summary(gam_simple_CON)
# 
# plot(gam_simple_CON)
# 
# gam_simple_CON2 <- gam(CONSUMPTION ~ clustering.m2 + s(POWER, bs="cr"), data = data.m2)
# summary(gam_simple_CON2)
# 
# gam_simple_CON3 <- gam(CONSUMPTION ~ clustering.w3 + s(POWER, bs="cr"), data = data.w3)
# summary(gam_simple_CON3)
# 
# ## PROVIAMO A SPIEGARE ANCHE IL RANGE IN FUNZIONE DELLA POTENZA
gam_simple_RANGE <- gam(RANGE ~ s(BATTERY_CAPACITY, bs="cr") + clustering.m2, data= data)
summary(gam_simple_RANGE)
# 
# # gam_simple_CON3 <- gam(CONSUMPTION ~ clustering.m3, data = data.m3)
# # summary(gam_simple_CON3)
# 
# ## da rivedere questo confronto ANOVA (1: non so se le assunzioni vengano rispettate, 2: vale davvero la pena tornare al modello più complesso)
# anova(gam_model2.drive,gam_model.drive,test="F")
# 
# ## proviamo a fittare con natural cubic splices
# #model_gam_ns <-   lm(data ~ ns(education, df = 3) + ns(POWER, df = 3), data = data
                     