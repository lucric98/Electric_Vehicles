setwd("C:/Users/UTENTE/Downloads/Nonparametric Statistics/Project/Electric_Vehicles")
data = evdatawithprices
library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)

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

# proviamo interactions

model_lm5=lm(Price ~ (Vehicle.Fuel.Euivalent 
             + Total.Power
             + Height
             + Length
             + Cargo.Volume)^2, data=data)
summary(model_lm5)

# tolgo height:length


git config --global user.email "alessiofrezza99@gmail.com"
git config --global user.name "Frezza99"

Omit --global 