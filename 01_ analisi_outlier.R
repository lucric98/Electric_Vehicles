library(dplyr)
path <- "/Users/luca/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Nonparam Project/DATASET/evdatawithprices.csv"
Vehicles <- read.csv(path,sep = ";")

## SUPPONIAMO CHE CARATTERIZZO I FURGONI A PARTIRE DA HEIGHT E MAX PAYLOAD
plot(Vehicles$Max..Payload)
#Furgoni -> Max..Payload > 800
pay_furgoni <- which(Vehicles$Max..Payload>800)

plot(Vehicles$Height)
#Furgoni -> Height > 1800
height_furgoni <- which(Vehicles$Height>1800)

diff1 <- setdiff(pay_furgoni,height_furgoni)
diff2 <- setdiff(height_furgoni,pay_furgoni)
## Diff2 da 5 veicoli:
# https://ev-database.org/car/1522/Peugeot-e-Rifter-Standard-50-kWh
# https://ev-database.org/car/1546/Citroen-e-Berlingo-M-50-kWh (5 posti, lo possiamo considerare come un furgone, io direi di si)
# https://ev-database.org/car/1651/Volkswagen-ID-Buzz-Pro (5 posti, lo possiamo considerare come un furgone, io direi di si)
# https://ev-database.org/car/1523/Peugeot-e-Rifter-Long-50-kWh
# https://ev-database.org/car/1547/Citroen-e-Berlingo-XL-50-kWh

## ATTENZIONE: CI SONO DUE COSE DA TENERE IN CONSIDERAZIONE: HEIGHT CONSIDERA 5 PUNTI IN PIU', RISPETTO AL MAX PAYLOAD. IO DIREI CHE COMUNQUE QUESTI PUNTI POSSONO ESSERE CARATTERIZZATI COME FURGONI

## SEATS >=5
ind_seats <- which(Vehicles$Seats>5)

diff1 <- setdiff(ind_seats,height_furgoni)
diff2 <- setdiff(height_furgoni,ind_seats)
## ATTENZIONE: CI SONO ALCUNE MACCHINE A 7 POSTI CHE PERO' NON SONO FURGONI. LA CARATTERISTICA CHE SECONDO ME REALMENTE CARATTERIZZA UN FURGONE E' L'ALTEZZA (>1800)


### OUTLIER ANALYSIS
library(dplyr)
library(ggplot2)

path <- "/Users/luca/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Nonparam Project/DATASET/evdatawithprices.csv"
Vehicles <- read.csv(path,sep = ";")

Vehicles$Seats <- as.factor(Vehicles$Seats)
Vehicles$Charge.Power <- as.factor(Vehicles$Charge.Power)
Vehicles$Drive <- as.factor(Vehicles$Drive)
Vehicles$Available <- as.factor(Vehicles$Available)

factors <- data.frame(Seats = Vehicles$Seats, Charge.Power = Vehicles$Charge.Power, Drive = Vehicles$Drive)

Vehicles <- select_if(Vehicles, is.numeric)
select(Vehicles, -c(id))

############################ UNIVARIATE BOXPLOT

#### Range And Power
boxplot(Vehicles$Combined...Cold.Weather)
which(Vehicles$Combined...Cold.Weather %in% boxplot.stats(Vehicles$Combined...Cold.Weather)$out)

boxplot(Vehicles$Combined...Mild.Weather)
which(Vehicles$Combined...Mild.Weather %in% boxplot.stats(Vehicles$Combined...Mild.Weather)$out)
## OUTLIER --> SUPERCAR
## https://ev-database.org/car/1483/Mercedes-EQS-450plus

boxplot(Vehicles$Total.Power)
which(Vehicles$Total.Power %in% boxplot.stats(Vehicles$Total.Power)$out)
## NO OUTLIER

boxplot(Vehicles$Total.Torque)
which(Vehicles$Total.Torque %in% boxplot.stats(Vehicles$Total.Torque)$out)
## NO OUTLIER

#### physics characteristics
boxplot(Vehicles$Acceleration.0...100.km.h)
which(Vehicles$Acceleration.0...100.km.h %in% boxplot.stats(Vehicles$Acceleration.0...100.km.h)$out)
## OUTLIER --> MACCHINA CON PERFORMANCE MOLTO LIMITATE
## https://ev-database.org/car/1705/Dacia-Spring-Electric

boxplot(Vehicles$Top.Speed)
which(Vehicles$Top.Speed %in% boxplot.stats(Vehicles$Top.Speed)$out)
## NO OUTLIER!

boxplot(Vehicles$Length)
which(Vehicles$Length %in% boxplot.stats(Vehicles$Length)$out)
## OUTLIER --> FIAT 500
# [1] "https://ev-database.org/car/1327/Fiat-500e-Hatchback-24-kWh"
# [2] "https://ev-database.org/car/1285/Fiat-500e-Hatchback-42-kWh"
# [3] "https://ev-database.org/car/1328/Fiat-500e-3plus1-42-kWh"   
# [4] "https://ev-database.org/car/1275/Fiat-500e-Cabrio-42-kWh"   

boxplot(Vehicles$Width)
which(Vehicles$Width %in% boxplot.stats(Vehicles$Width)$out)
## OUTLIER --> FIAT 500 E DACIA SPRING
# [1] "https://ev-database.org/car/1327/Fiat-500e-Hatchback-24-kWh"
# [2] "https://ev-database.org/car/1285/Fiat-500e-Hatchback-42-kWh"
# [3] "https://ev-database.org/car/1705/Dacia-Spring-Electric"     
# [4] "https://ev-database.org/car/1328/Fiat-500e-3plus1-42-kWh"   
# [5] "https://ev-database.org/car/1275/Fiat-500e-Cabrio-42-kWh" 

boxplot(Vehicles$Height)
which(Vehicles$Height %in% boxplot.stats(Vehicles$Height)$out)
## OUTLIER --> FURGONI
# [1] "https://ev-database.org/car/1522/Peugeot-e-Rifter-Standard-50-kWh"      
# [2] "https://ev-database.org/car/1721/Fiat-E-Ulysse-L2-50-kWh"               
# [3] "https://ev-database.org/car/1595/Citroen-e-Jumpy-Combi-M-50-kWh"        
# [4] "https://ev-database.org/car/1596/Citroen-e-Jumpy-Combi-XL-50-kWh"       
# [5] "https://ev-database.org/car/1610/Toyota-PROACE-Shuttle-L-50-kWh"        
# [6] "https://ev-database.org/car/1351/Peugeot-e-Traveller-Standard-50-kWh"   
# [7] "https://ev-database.org/car/1240/Mercedes-EQV-300-Long"                 
# [8] "https://ev-database.org/car/1342/Citroen-e-SpaceTourer-XL-50-kWh"       
# [9] "https://ev-database.org/car/1605/Peugeot-e-Expert-Combi-Standard-50-kWh"
# [10] "https://ev-database.org/car/1608/Peugeot-e-Expert-Combi-Long-75-kWh"    
# [11] "https://ev-database.org/car/1606/Peugeot-e-Expert-Combi-Long-50-kWh"    
# [12] "https://ev-database.org/car/1651/Volkswagen-ID-Buzz-Pro"                
# [13] "https://ev-database.org/car/1514/Toyota-PROACE-Verso-M-75-kWh"          
# [14] "https://ev-database.org/car/1723/Fiat-E-Ulysse-L3-50-kWh"               
# [15] "https://ev-database.org/car/1602/Opel-Vivaro-e-Combi-M-75-kWh"          
# [16] "https://ev-database.org/car/1612/Toyota-PROACE-Shuttle-L-75-kWh"        
# [17] "https://ev-database.org/car/1611/Toyota-PROACE-Shuttle-M-75-kWh"        
# [18] "https://ev-database.org/car/1609/Toyota-PROACE-Shuttle-M-50-kWh"        
# [19] "https://ev-database.org/car/1523/Peugeot-e-Rifter-Long-50-kWh"          
# [20] "https://ev-database.org/car/1315/Mercedes-EQV-300-Extra-Long"           
# [21] "https://ev-database.org/car/1515/Toyota-PROACE-Verso-L-75-kWh"          
# [22] "https://ev-database.org/car/1352/Peugeot-e-Traveller-Long-50-kWh"       
# [23] "https://ev-database.org/car/1601/Opel-Vivaro-e-Combi-L-50-kWh"          
# [24] "https://ev-database.org/car/1607/Peugeot-e-Expert-Combi-Standard-75-kWh"
# [25] "https://ev-database.org/car/1344/Citroen-e-SpaceTourer-XL-75-kWh"       
# [26] "https://ev-database.org/car/1598/Citroen-e-Jumpy-Combi-XL-75-kWh"       
# [27] "https://ev-database.org/car/1354/Peugeot-e-Traveller-Long-75-kWh"       
# [28] "https://ev-database.org/car/1600/Opel-Vivaro-e-Combi-M-50-kWh"          
# [29] "https://ev-database.org/car/1341/Citroen-e-SpaceTourer-M-50-kWh"        
# [30] "https://ev-database.org/car/1724/Fiat-E-Ulysse-L3-75-kWh"               
# [31] "https://ev-database.org/car/1597/Citroen-e-Jumpy-Combi-M-75-kWh"        
# [32] "https://ev-database.org/car/1722/Fiat-E-Ulysse-L2-75-kWh"               
# [33] "https://ev-database.org/car/1343/Citroen-e-SpaceTourer-M-75-kWh"        
# [34] "https://ev-database.org/car/1603/Opel-Vivaro-e-Combi-L-75-kWh"          
# [35] "https://ev-database.org/car/1353/Peugeot-e-Traveller-Standard-75-kWh" 

boxplot(Vehicles$Cargo.Volume)
which(Vehicles$Cargo.Volume %in% boxplot.stats(Vehicles$Cargo.Volume)$out)
## OUTLIER --> QUI GLI OUTLIER NON SONO SOLO FURGONI
# [1] "https://ev-database.org/car/1596/Citroen-e-Jumpy-Combi-XL-50-kWh"    
# [2] "https://ev-database.org/car/1610/Toyota-PROACE-Shuttle-L-50-kWh"     
# [3] "https://ev-database.org/car/1240/Mercedes-EQV-300-Long"              
# [4] "https://ev-database.org/car/1342/Citroen-e-SpaceTourer-XL-50-kWh"    
# [5] "https://ev-database.org/car/1608/Peugeot-e-Expert-Combi-Long-75-kWh" 
# [6] "https://ev-database.org/car/1606/Peugeot-e-Expert-Combi-Long-50-kWh" 
# [7] "https://ev-database.org/car/1651/Volkswagen-ID-Buzz-Pro"             
# [8] "https://ev-database.org/car/1723/Fiat-E-Ulysse-L3-50-kWh"            
# [9] "https://ev-database.org/car/1612/Toyota-PROACE-Shuttle-L-75-kWh"     
# [10] "https://ev-database.org/car/1523/Peugeot-e-Rifter-Long-50-kWh"       
# [11] "https://ev-database.org/car/1315/Mercedes-EQV-300-Extra-Long"        
# [12] "https://ev-database.org/car/1515/Toyota-PROACE-Verso-L-75-kWh"       
# [13] "https://ev-database.org/car/1352/Peugeot-e-Traveller-Long-50-kWh"    
# [14] "https://ev-database.org/car/1601/Opel-Vivaro-e-Combi-L-50-kWh"       
# [15] "https://ev-database.org/car/1619/Tesla-Model-Y-Long-Range-Dual-Motor"
# [16] "https://ev-database.org/car/1344/Citroen-e-SpaceTourer-XL-75-kWh"    
# [17] "https://ev-database.org/car/1183/Tesla-Model-Y-Performance"          
# [18] "https://ev-database.org/car/1598/Citroen-e-Jumpy-Combi-XL-75-kWh"    
# [19] "https://ev-database.org/car/1354/Peugeot-e-Traveller-Long-75-kWh"    
# [20] "https://ev-database.org/car/1724/Fiat-E-Ulysse-L3-75-kWh"            
# [21] "https://ev-database.org/car/1603/Opel-Vivaro-e-Combi-L-75-kWh"       
# [22] "https://ev-database.org/car/1547/Citroen-e-Berlingo-XL-50-kWh"

boxplot(Vehicles$Gross.Vehicle.Weight..GVWR.)
which(Vehicles$Gross.Vehicle.Weight..GVWR. %in% boxplot.stats(Vehicles$Gross.Vehicle.Weight..GVWR.)$out)
# [1] "https://ev-database.org/car/1705/Dacia-Spring-Electric"

## Battery information
boxplot(Vehicles$Battery.Capacity)
which(Vehicles$Battery.Capacity %in% boxplot.stats(Vehicles$Battery.Capacity)$out)
# NO OUTLIER

boxplot(Vehicles$Charge.Speed)
which(Vehicles$Charge.Speed %in% boxplot.stats(Vehicles$Charge.Speed)$out)
##OUTLIER --> AUTO CHE IMPIEGANO POCO PER ESSERE CARICATE. COSA NE DICIAMO?
# [1] "https://ev-database.org/car/1748/Smart-1-Brabus"                  
# [2] "https://ev-database.org/car/1588/Renault-Megane-E-Tech-EV60-130hp"
# [3] "https://ev-database.org/car/1536/Renault-Megane-E-Tech-EV40-130hp"
# [4] "https://ev-database.org/car/1164/Renault-Zoe-ZE50-R110"           
# [5] "https://ev-database.org/car/1521/Renault-Megane-E-Tech-EV60-220hp"
# [6] "https://ev-database.org/car/1205/Renault-Zoe-ZE50-R135"  

boxplot(Vehicles$Fastcharge.Speed)
which(Vehicles$Fastcharge.Speed %in% boxplot.stats(Vehicles$Fastcharge.Speed)$out)
# [1] "https://ev-database.org/car/1394/Porsche-Taycan-Plus"

## Price and Consumption
boxplot(Vehicles$Consumption)
which(Vehicles$Consumption %in% boxplot.stats(Vehicles$Consumption)$out)

boxplot(Vehicles$Vehicle.Fuel.Euivalent)
which(Vehicles$Vehicle.Fuel.Euivalent %in% boxplot.stats(Vehicles$Vehicle.Fuel.Euivalent)$out)
## OUTLIER --> FURGONE
# [1] "https://ev-database.org/car/1240/Mercedes-EQV-300-Long"      
# [2] "https://ev-database.org/car/1315/Mercedes-EQV-300-Extra-Long"

boxplot(Vehicles$Price)
which(Vehicles$Price %in% boxplot.stats(Vehicles$Price)$out)
## OUTLIER --> SUPERCAR
# [1] "https://ev-database.org/car/1229/Porsche-Taycan-Turbo"                
# [2] "https://ev-database.org/car/1438/Porsche-Taycan-Turbo-Cross-Turismo"  
# [3] "https://ev-database.org/car/1625/Porsche-Taycan-Turbo-Sport-Turismo"  
# [4] "https://ev-database.org/car/1624/Porsche-Taycan-4S-Plus-Sport-Turismo"
# [5] "https://ev-database.org/car/1484/Mercedes-EQS-580-4MATIC"             
# [6] "https://ev-database.org/car/1537/Mercedes-EQS-AMG-53-4MATICplus"      
# [7] "https://ev-database.org/car/1560/Porsche-Taycan-GTS"                  
# [8] "https://ev-database.org/car/1700/Mercedes-EQS-500-4MATIC"             
# [9] "https://ev-database.org/car/1590/BMW-iX-M60"                          
# [10] "https://ev-database.org/car/1674/Mercedes-EQS-SUV-450-4MATIC"         
# [11] "https://ev-database.org/car/1673/Mercedes-EQS-SUV-450plus"            
# [12] "https://ev-database.org/car/1153/Audi-e-tron-GT-RS"                   
# [13] "https://ev-database.org/car/1437/Porsche-Taycan-4S-Cross-Turismo"     
# [14] "https://ev-database.org/car/1439/Porsche-Taycan-Turbo-S-Cross-Turismo"
# [15] "https://ev-database.org/car/1626/Porsche-Taycan-Turbo-S-Sport-Turismo"
# [16] "https://ev-database.org/car/1238/Porsche-Taycan-4S-Plus"              
# [17] "https://ev-database.org/car/1561/Porsche-Taycan-GTS-Sport-Turismo"    
# [18] "https://ev-database.org/car/1676/BMW-i7-xDrive60"                     
# [19] "https://ev-database.org/car/1699/Mercedes-EQS-450-4MATIC"             
# [20] "https://ev-database.org/car/1116/Porsche-Taycan-Turbo-S"       
############################ BAGPLOT
library(DepthProc)
library(aplpack)


range <- as.data.frame(cbind(Vehicles$City...Cold.Weather, Vehicles$Highway...Cold.Weather, Vehicles$Combined...Cold.Weather, Vehicles$Highway...Mild.Weather, Vehicles$City...Mild.Weather, Vehicles$Combined...Mild.Weather, Vehicles$Electric.Range))
bagplot.pairs(range)
# cor(range)
## These variables are all perfect linear combinations --> we keep just the the Electric Range
physics <- as.data.frame(cbind(ACC = Vehicles$Acceleration.0...100.km.h, TOP_SPEED = Vehicles$Top.Speed, LENGTH = Vehicles$Length, WIDTH = Vehicles$Width, HEIGHT = Vehicles$Height, WHEELBASE = Vehicles$Wheelbase, GVWR = Vehicles$Gross.Vehicle.Weight..GVWR., PAYLOAD = Vehicles$Max..Payload, CARGO_VOL = Vehicles$Cargo.Volume, POWER = Vehicles$Total.Power, TORQUE = Vehicles$Total.Torque))
bagplot.pairs(physics)
## variabili da tenere: ACCELERATION, LENGTH, HEIGHT. Dopo di ché bisogna fare un certo tipo di analisi per decidere quale variabile utilizzare tra GVWR, PAYLOAD e CARGO_VOLUME. Probabilmete GVWR è la variabile che introduce meno informazione, ma meno soggetta al problema dei furgoni. Le altre due  presentano un comportamente simile, come le analiziamo?
pairs(physics)

battery <- as.data.frame(cbind(CHARGE_SPEED = Vehicles$Charge.Speed, BATTERY_CAPACITY = Vehicles$Battery.Capacity, FASTCHARGE_SPEED = Vehicles$Fastcharge.Speed))
bagplot.pairs(battery)
## queste variabili non sono molto correlate, però CHARGE SPEED presenta outlier piuttosto pesanti, forse varrebbe la pena rimuoverla e tenere le altre due?

price_con <- as.data.frame(cbind(Vehicles$Price,Vehicles$Consumption,Vehicles$Vehicle.Fuel.Euivalent))
bagplot.pairs(price_con)

### Analizziamo le caratteristiche selezionate insieme
data <- as.data.frame(cbind(ACC = Vehicles$Acceleration.0...100.km.h, LENGTH = Vehicles$Length, HEIGHT = Vehicles$Height, PAYLOAD = Vehicles$Max..Payload, CARGO_VOL = Vehicles$Cargo.Volume,
                           RANGE = Vehicles$Electric.Range, 
                           CHARGE_SPEED = Vehicles$Charge.Speed, BATTERY_CAPACITY = Vehicles$Battery.Capacity, FASTCHARGE_SPEED = Vehicles$Fastcharge.Speed,
                           PRICE = Vehicles$Price, CONSUMPTION = Vehicles$Consumption, 
                           POWER = Vehicles$Total.Power))
pairs(data)
bagplot.pairs(data)


ev <- 








