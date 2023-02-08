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

### APPLY CONFORMAL PREDICTION - Versione 1 - use the data at our disposal
X <- data.frame(PRICE = data$PRICE, CONSUMPTION = data$CONSUMPTION)
#X <- scale(X)
n <- dim(X)[1]
wrapper_multi_conf=function(test_point){
  
  newdata=rbind(test_point,X)
  # L^2 norm
  #depth_surface_vec=rowSums(t(t(newdata)-colMeans(newdata))^2) 
  # mahalanobis
  depth_surface_vec= mahalanobis(newdata,colMeans(newdata),cov = cov(newdata))
  
  sum(depth_surface_vec[-1]>=depth_surface_vec[1])/(n+1)
}

n_grid = 200
grid_factor = 1.25
#test_grid_x=seq(-grid_factor*max(abs(X[,1])),+grid_factor*max(abs(X[,1])),length.out = n_grid)
#test_grid_y=seq(-grid_factor*max(abs(X[,2])),+grid_factor*max(abs(X[,2])),length.out = n_grid)
#xy_surface=expand.grid(test_grid_x,test_grid_y)


test_grid_x <- seq(min(X[,1]) - 0.25*diff(range(X[,1])), max(X[,1]) +
                     0.25*diff(range(X[,1])), length = 200)
test_grid_y <- seq(min(X[,2]) - 0.25*diff(range(X[,2])), max(X[,2]) +
                     0.25*diff(range(X[,2])), length = 200)
xy_surface=expand.grid(test_grid_x,test_grid_y)

p.value=pbapply(xy_surface,1,wrapper_multi_conf)

data_plot=cbind(p.value,xy_surface)
p_set=xy_surface[p.value>alpha,]
poly_points=p_set[chull(p_set),]

ggplot() + 
  geom_tile(data=data_plot, aes(Var1, Var2, fill= p.value)) +
  geom_point(data=data.frame(X), aes(PRICE, CONSUMPTION)) + 
  geom_polygon(data=poly_points,aes(Var1,Var2),color='red',size=1,alpha=alpha)



### APPLY CONFORMAL PREDICTION - Versione 2 - Exploit the model we have built in first phase
seed = 26111992
## CLUSTERING
data.e <- daisy(data,metric = "gower")
data.em <- hclust(data.e, method = "mcquitty")
clustering.m2 <- cutree(data.em,k=2)

scaled_data <- data
scaled_data[,1:12] <- scale(data[,1:12])
## MODELLO PRICE
gam_PRICE <- gam(PRICE ~ s(LENGTH, bs="cr") + s(HEIGHT,bs="cr") + s(I(LENGTH*HEIGHT),bs="cr") + clustering.m2, data=scaled_data)
summary(gam_PRICE)
## MODELLO CONSUMPTION
gam_CONSUMPTION <- gam(CONSUMPTION ~ s(HEIGHT, bs="cr") + s(LENGTH, bs="cr") + s(I(HEIGHT*LENGTH),bs="cr") + Drive, data=scaled_data)
summary(gam_CONSUMPTION)

## Let's build a Confromal prediction region exploiting the models: We buil a conformal prediction interval for the entire price of an electric vehicle using our models and just the infromation about lenght and HEIGHT of the vehicle
## PREDICT THE PRICE
# x2.grid=seq(range(x2)[1],range(x2)[2],length.out = 100)
# x1.grid=seq(range(x1)[1],range(x1)[2],length.out = 100)
# grid=expand.grid(x2.grid,x1.grid)
# names(grid)=c('x2','x1')
# 
# pred=predict(model_gam,newdata=grid)
# 
# train_gam=function(x,y,out=NULL){
#   colnames(x)=c('var1','var2')
#   train_data=data.frame(y,x)
#   model_gam=gam(y ~ s(var1,bs='cr') + s(var2,bs='cr'),data=train_data)
# }
# 
# predict_gam=function(obj, new_x){
#   new_x=data.frame(new_x)
#   colnames(new_x)=c('var1','var2')
#   predict.gam(obj,new_x)
# }
# 
# ## PREDICT THE CONSUMPTION
# x2.grid=seq(range(x2)[1],range(x2)[2],length.out = 100)
# x1.grid=seq(range(x1)[1],range(x1)[2],length.out = 100)
# grid=expand.grid(x2.grid,x1.grid)
# names(grid)=c('x2','x1')
# 
# pred=predict(model_gam,newdata=grid)
# 
# train_gam=function(x,y,out=NULL){
#   colnames(x)=c('var1','var2')
#   train_data=data.frame(y,x)
#   model_gam=gam(y ~ s(var1,bs='cr') + s(var2,bs='cr'),data=train_data)
# }
# 
# predict_gam=function(obj, new_x){
#   new_x=data.frame(new_x)
#   colnames(new_x)=c('var1','var2')
#   predict.gam(obj,new_x)
# }
## CONFORMAL PREDICTION REGION
c_preds=conformal.pred(cbind(x2,x1),y,c(median(x2),median(x1)),alpha=alpha,verbose=T,train.fun = train_gam ,predict.fun = predict_gam,num.grid.pts = 200)
c_preds
