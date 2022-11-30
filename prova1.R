# setwd("C:/Users/UTENTE/Downloads/Nonparametric Statistics/Project/Electric_Vehicles")
data = evdatawithprices
library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(mgcv)

attach(data)

# simple linear regression
model_lm1=lm(Price ~ Electric.Range 
                  + Vehicle.Fuel.Euivalent 
                  + Charge.Speed 
                  + Total.Power
                  + Height
                  + Length
                  + Max..Payload
                  + Cargo.Volume, data=data)
summary(model_lm1)

# tolgo charge.speed
model_lm2=lm(Price ~ Electric.Range 
             + Vehicle.Fuel.Euivalent 
             + Total.Power
             + Height
             + Length
             + Max..Payload
             + Cargo.Volume, data=data)
summary(model_lm2)

# tolgo max.payload
model_lm3=lm(Price ~ Electric.Range 
             + Vehicle.Fuel.Euivalent 
             + Total.Power
             + Height
             + Length
             + Cargo.Volume, data=data)
summary(model_lm3)

# tolgo electric.range
model_lm4=lm(Price ~ Vehicle.Fuel.Euivalent 
             + Total.Power
             + Height
             + Length
             + Cargo.Volume, data=data)
summary(model_lm4)

# per ora sembra essere il miglior modello (linear regression)
# ma le assunzioni per regressione lineare non sono soddisfatte (come ci aspettavamo)
plot(model_lm4)
shapiro.test(residuals(model_lm4))

#GLM
# (1)cubic spline basis
model_gam1=gam(Price ~ s(Electric.Range,bs='cr') 
                    + s(Vehicle.Fuel.Euivalent,bs='cr')
                    + s(Charge.Speed,bs='cr') 
                    + s(Total.Power,bs='cr') 
                    + s(Height,bs='cr') 
                    + s(Length,bs='cr') 
                    + s(Max..Payload,bs='cr') 
                    + s(Cargo.Volume,bs='cr'), data=data
              )
summary(model_gam1)
# alto R-sq 0.971, ma max.payload sembra poco significativo

# tolgo max.paypload
model_gam2=gam(Price ~ s(Electric.Range,bs='cr') 
               + s(Vehicle.Fuel.Euivalent,bs='cr')
               + s(Charge.Speed,bs='cr') 
               + s(Total.Power,bs='cr') 
               + s(Height,bs='cr') 
               + s(Length,bs='cr') 
               + s(Cargo.Volume,bs='cr'), data=data
              )
summary(model_gam2)
# quasi identico R-sq 0.969, tutte variabili significative

# residuals
hist(model_gam2$residuals)
qqnorm(model_gam2$residuals)

# normality test
shapiro.test(model_gam2$residuals)

# (2)natural cubic splines
model_gam_ns =
  lm(Price ~ ns(Electric.Range,df=8) 
            + ns(Vehicle.Fuel.Euivalent,df=8)
            + ns(Charge.Speed,df=8) 
            + ns(Total.Power,df=8) 
            + ns(Height,df=8) 
            + ns(Length,df=8) 
            + ns(Cargo.Volume,df=8), data=data
     )
# summary(model_gam_ns)

# scatterplot for the two models
plot(model_gam_ns$residuals,model_gam2$residuals)
cor(model_gam_ns$residuals,model_gam2$residuals)

# Consider model with cubic spline basis
# I look at the contribution of a single predictor 
# holding all the others fixed
plot(model_gam2)