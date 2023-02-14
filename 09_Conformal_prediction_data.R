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

########### TOTAL_PRICE = PRICE + CONSUMPTION\*KM_LIFE*LCOC

# - PRICE <- Obtained thanks to Conformal Prediction thanks to our model
# - CONSUMPTION <- Obtained thanks to Conformal Prediction thanks to our model
# - KM_LIFE <- Life of the electric vehicle battery expressed in kilometers. Il ragionamento che facciamo per ottenere questo parametro è il seguente: abbiamo utilizzato la legge di decadimento temporale per la batteria di un veicolo elettrico accoppiata con i dati di decadimento finora osservati in alcune automobili (manually collected). Uno volta determinato il decadimento temporale (in anni) abbiamo definito 3 profili di utilizzo (in base al numero di kilometri che vengono percorsi da un'automobile in un determinato anno, visto che normalmente un'automobile non viene utilizzata per milioni di kilometri in un determinato anno)
# - LCOC <- Levelized costs of charging . Questo parametro si basa sul risultato di un articolo (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9458728/#CR7) e si basa sul fatto che per stimare il costo di ricarica di un veicolo elettrico non è sufficiente stimare il costo dell'elettricità perché ci sono molti fattori che entrano in gioco in questa stima. Quindi ci rifacciamo ad un'analisi già effettuata che ci garantisce 4 profili di costo di ricarica per la macchina elettrica in germania.

########### APPLY CONFORMAL PREDICTION - Versione 1 - use the data and the model at our disposal
X <- data.frame(PRICE = data$PRICE, CONSUMPTION = data$CONSUMPTION)
#X <- scale(X)
n_b = n <- dim(X)[1]
n_grid <- 100
wrapper_multi_conf=function(test_point){
  
  newdata=rbind(test_point,X)
  # L^2 norm
  #depth_surface_vec=rowSums(t(t(newdata)-colMeans(newdata))^2) 
  # mahalanobis
  depth_surface_vec= mahalanobis(newdata,colMeans(newdata),cov = cov(newdata))
  
  sum(depth_surface_vec[-1]>=depth_surface_vec[1])/(n+1)
}

test_grid_price <- seq(min(X[,1]) - 0.05*diff(range(X[,1])), 
                       max(X[,1]) + 0.05*diff(range(X[,1])), 
                       length = n_grid)
test_grid_cons <- seq(min(X[,2]) - 0.05*diff(range(X[,2])), 
                      max(X[,2]) + 0.05*diff(range(X[,2])), 
                      length = n_grid)
xy_surface=expand.grid(test_grid_price,test_grid_cons)
names(xy_surface) <- c("PRICE", "CONSUMPTION")

p.value=pbapply(xy_surface,1,wrapper_multi_conf)

data_plot=cbind(p.value,xy_surface)
alpha <- 0.1
p_set=xy_surface[p.value>alpha,]
poly_points=p_set[chull(p_set),]
## Conformal prediction region per prezzo e consumption (non so a cosa possa servire ma potrebbe tornarci utile)
ggplot() + 
  geom_tile(data=data_plot, aes(PRICE, CONSUMPTION, fill= p.value)) +
  geom_point(data=data.frame(X), aes(PRICE, CONSUMPTION)) + 
  geom_polygon(data=poly_points,aes(PRICE,CONSUMPTION),color='red',size=1,alpha=alpha)

### CALCOLIAMO IL CONFORMAL PREDICTION REGION PER IL PREZZO completo DI UNA AUMOBILE 
### TOLGO GLI OUTLIER: LE OSSERVAZIONI FUORI DALLA REGIONE DI CONFORMAL PREDICTION 0.9
out_price <- which(data$PRICE %in% boxplot(data$PRICE)$out)
out_cons <- which(data$CONSUMPTION %in% boxplot(data$CONSUMPTION)$out)
out_ind <- union(out_price, out_cons)
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
km_life <- 12*20000
lcoc <- 0.330/1000

datawo <- data[-out_ind,]
tot_price <- datawo$PRICE + datawo$CONSUMPTION*km_life*lcoc
n_grid <- 1000
tot_price_grid <- seq(min(tot_price), max(tot_price),length = n_grid)

pval_fun=numeric(n_grid)

wrapper_knn=function(grid_point){
  aug_y=c(grid_point,tot_price)
  ncm=kNNdist(matrix(aug_y),100)
  sum((ncm[-1]>=ncm[1]))/(n_b+1)
} 
wrapper_full_median=function(grid_point){
  x.obs.aug=c(grid_point,tot_price)
  mu=median(x.obs.aug)     # WHY ISN'T IT REMOVING THE ith OBSERVATION?
  ncm=abs(mu - x.obs.aug)  
  sum((ncm[-1]>=ncm[1]))/(n+1)  # first obs refers to grid_point
}
pval_fun = pbsapply(tot_price_grid,wrapper_knn)
pval_fun = pbsapply(tot_price_grid,wrapper_full_median)
# Plot the p-values
plot(tot_price_grid, pval_fun, type='l', ylim=c(0,1))
abline(h=c(0,1))
abline(h=alpha, col='red', lty=2)
points(tot_price, numeric(length(tot_price)), pch=3)

# Compute the Prediction Interval
alpha <- 0.1
PI.grid <- tot_price_grid[which(pval_fun >= alpha)]
PI <- c(min(PI.grid), max(PI.grid))
PI
abline(v = PI, col='red')
points(PI.grid, numeric(length(PI.grid)), pch=16, col='red')

