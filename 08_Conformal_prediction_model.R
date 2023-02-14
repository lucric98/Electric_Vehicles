library(dplyr)
library(ggplot2)
library(splines)
library(pbapply)
library(mvtnorm)
library(rgl)
library(car)
library(cluster)
library(dbscan)
library(ISLR2)
library(mgcv)
library(DepthProc)
library(aplpack)
library(mgcv)
library(progress)

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
data <- na.omit(data)

seed = 26111992
## CLUSTERING
data.e <- daisy(data,metric = "gower")
data.em <- hclust(data.e, method = "mcquitty")
clustering.m2 <- cutree(data.em,k=2)
data$Van <- as.factor(clustering.m2)
########### TOTAL_PRICE = PRICE + CONSUMPTION\*KM_LIFE*LCOC

# - PRICE <- Obtained thanks to Conformal Prediction thanks to our model
# - CONSUMPTION <- Obtained thanks to Conformal Prediction thanks to our model
# - KM_LIFE <- Life of the electric vehicle battery expressed in kilometers. Il ragionamento che facciamo per ottenere questo parametro è il seguente: abbiamo utilizzato la legge di decadimento temporale per la batteria di un veicolo elettrico accoppiata con i dati di decadimento finora osservati in alcune automobili (manually collected). Uno volta determinato il decadimento temporale (in anni) abbiamo definito 3 profili di utilizzo (in base al numero di kilometri che vengono percorsi da un'automobile in un determinato anno, visto che normalmente un'automobile non viene utilizzata per milioni di kilometri in un determinato anno)
# - LCOC <- Levelized costs of charging . Questo parametro si basa sul risultato di un articolo (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9458728/#CR7) e si basa sul fatto che per stimare il costo di ricarica di un veicolo elettrico non è sufficiente stimare il costo dell'elettricità perché ci sono molti fattori che entrano in gioco in questa stima. Quindi ci rifacciamo ad un'analisi già effettuata che ci garantisce 4 profili di costo di ricarica per la macchina elettrica in germania.

### Strategia: a partire da due caratteristiche fisica (come altezza e lunghezza, trazione e distinzione Van/no van) proviamo a costruire intervalli di conformal prediction per il prezzo totale dell'automobile

# La griglia per predire prezzo e consumi dell'automobile va costruita in modo molto intelligente 
plot(Vehicles$Length,Vehicles$Height,col=Vehicles$Drive)
# Non c'è relazione lineare ttra prezzo e consumption!!!
plot(Vehicles$Price,Vehicles$Consumption,col=Vehicles$Drive)

# ## se voglio predire l'altezza delle macchine devo arrivare fino all'auto più alta, tralasciando i furgoni
# # GRIGLIA PER LE AUTOMOBILI
# length.grid <- seq(min(Vehicles$Length), max(Vehicles$Length), length = 50)
# height.grid <- seq(min(Vehicles$Height), max(Vehicles$Height[Vehicles$Height<1800]), length = 50)
# 
# grid <- expand.grid(LENGTH = length.grid, HEIGHT = height.grid, Drive = c("Front", "Rear","AWD"), Van="1")
# # GRIGLIA PER I VAN
# length.grid_van <- seq(min(Vehicles$Length[Vehicles$Height>1800]),
#                        max(Vehicles$Length[Vehicles$Height>1800]),
#                        length = 20)
# height.grid_van <- seq(min(Vehicles$Height[Vehicles$Height>1800]),
#                        max(Vehicles$Height[Vehicles$Height>1800]),
#                        length = 20)
# grid_van <- expand.grid(LENGTH = length.grid_van, HEIGHT = height.grid_van, Drive = c("Front"), Van="2")
# # GRIGLIA TOTALE
# total_pred_grid <- rbind(grid,grid_van)
# n <- dim(total_pred_grid)[1]

# MODELLI GAMs per prezzo e CONSUMPTION

## predico prezzo
PRICE_model <- gam(PRICE ~ s(LENGTH, bs="cr") + s(HEIGHT,bs="cr") + s(I(LENGTH*HEIGHT),bs="cr"), data=data)
summary(PRICE_model)

# pred_PRICE <- predict(PRICE_model,newdata=total_pred_grid)
## predico consumption
CONSUMPTION_model <- gam(CONSUMPTION ~ s(LENGTH, bs="cr") + s(HEIGHT,bs="cr") + s(I(LENGTH*HEIGHT),bs="cr"), data=data)
summary(CONSUMPTION_model)

# pred_CONSUMPTION <- predict(CONSUMPTION_model,newdata=total_pred_grid)

# TOTAL_PRICE = PRICE + CONSUMPTION (Wh/km) *KM_LIFE (km) *LCOC (€/KWh)

# KM_LIFE = VITA_BATTERIA*KM_ANNUI 
# KM_ANNUI <- c(10000,13220,20000)
# VITA_BATTERIA <- c(8,12,17)
# LCOC = Usare dati file excel (€/KWh)
## AVERAGE_USER: 0.284, 0.330, 0.375
## WALLBOX USER: 0.317, 0.363, 0.408
## WALLBOX USER WITH PV: 0.285, 0.323, 0.360
## COMMERCIAL USER: 0.345, 0.383, 0.422
## SOCKET USER: 0.182, 0.228, 0.273	

## queste variabili vanno settate facendo considerando diverse situazioni!
km_life <- 12*20000
lcoc <- 0.323/1000
new_data <- data.frame(HEIGHT = 1500, LENGTH = 4500)

# tot_price.grid <- data$PRICE + data$CONSUMPTION*km_life*lcoc


train_gam=function(x,y,out=NULL){
  colnames(x)=c('LENGTH','HEIGHT')
  train_data=data.frame(y,x)
  model_gam=gam(y ~ s(LENGTH,bs='cr') + s(HEIGHT,bs='cr') + s(I(LENGTH*HEIGHT),bs="cr"), data=train_data)
}

predict_gam=function(obj, new_x){
  new_x=data.frame(new_x)
  colnames(new_x)=c('LENGTH','HEIGHT')
  predict.gam(obj,new_x)
}

dati <- data.frame(PRICE = data$PRICE, CONSUMPTION = data$CONSUMPTION, LENGTH = data$LENGTH, HEIGHT = data$HEIGHT)
n <- dim(dati)[1]
B <- 500
x0 <- c(median(dati$LENGTH), median(dati$HEIGHT))
new_data <- data.frame(LENGTH = x0[1], HEIGHT = x0[2])
alpha <- 0.1

T.price.pred <- predict(PRICE_model,new_data)
T.price.boot <- numeric(B)

T.cons.pred <- predict(CONSUMPTION_model, new_data)
T.cons.boot <- numeric(B)

pb <- progress_bar$new(
  format = "  processing [:bar] :percent eta: :eta",
  total = B, clear = FALSE)

for(i in 1:B){
  ind.boot <- sample(1:n, replace = TRUE)
  
  dati.boot <- dati[ind.boot,]
  price_preds=conformal.pred(dati.boot[,3:4],
                            y = dati.boot$PRICE,
                            x0 = x0,
                            alpha=alpha,
                            verbose=F,
                            train.fun = train_gam,
                            predict.fun = predict_gam)
  
  T.price.boot[i] <- price_preds$pred
  
  cons_preds=conformal.pred(dati.boot[,3:4],
                         y = dati.boot$CONSUMPTION,
                         x0 = x0,
                         alpha=alpha,
                         verbose=F,
                         train.fun = train_gam,
                         predict.fun = predict_gam)
  
  T.cons.boot[i] <- cons_preds$pred
  pb$tick()
}

## PRICE
right.quantile <- quantile(T.price.boot, 1 - alpha/2)
left.quantile  <- quantile(T.price.boot, alpha/2)
CI.RP_price <- c(T.price.pred - (right.quantile - T.price.pred), T.price.pred, T.price.pred - (left.quantile - T.price.pred))
names(CI.RP_price)=c('lwr','lvl','upr')
CI.RP_price
## CONSUMPTION
right.quantile <- quantile(T.cons.boot, 1 - alpha/2)
left.quantile  <- quantile(T.cons.boot, alpha/2)
CI.RP_cons <- c(T.cons.pred - (right.quantile - T.cons.pred), T.cons.pred, T.cons.pred - (left.quantile - T.cons.pred))
names(CI.RP_cons)=c('lwr','lvl','upr')
CI.RP_cons

total_price <- CI.RP_price + CI.RP_cons*km_life*lcoc
total_price
