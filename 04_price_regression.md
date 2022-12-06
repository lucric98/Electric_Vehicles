# GAMs: PRICE
Definiamo un modello di regressione nonparametrico per il **prezzo** (tenendo in considerazione le seguendi covariate: ACC, LENGTH, HEIGHT, PAYLOAD, CARGO_VOL, RANGE, POWER, CHARGE_SPEED, BATTERY_CAPACITY, FASTCHARGE_SPEED, PRICE, CONSUMPTION, Drive, Seats, Charge_power e i 3 clustering che abbiamo selezionato in prima battuta).

## 1
Fitto un primo modello per farmi un'idea (tutte le categoriche fisiche). 
Il modello viene fittato utilizzando **cubic spline** per tutte le caratteristiche fisiche delle automobili e cercando di tenere in considerazione i fattori in maniera distinta (l'unico che sembra essere utile è la trazione!) 

## 2 
Smetto di considerare i fattori che non sono di nostro interesse (i _clustering_, tranne w3 per cui va fatta un'analisi a parte, _Seats_, _Charge_power_).
Fitto un nuovo modello con tutte le **variabili fisiche** e come fattore tengo in considerazione solo la **trazione**.

## 3
Riduco il modello in modo da tenere in considerazione solo covariate continue che possono essere significative per modellare il prezzo:
**ACC, LENGTH, HEIGHT, RANGE, POWER, BATTERY_CAPACITY and CARGO_VOLUME**. A questo viene aggiunto la trazione (**Drive**). In questo modello per tutte le covariate sembra necessaria la modellazione nonlineare. L'unica variabile che entra in maniera lineare è la trazione.
Mi piacerebbe capire se fosse possibile introdurre delle interazioni non lineari (modelli ad effetti misti?). Magari va fatto un ragionamento di questo tipo: posso, sfruttando la trazione e/o i clustering in questo tipo di modelli e le interazioni di questi fattori con le variabili è possibile usare una modellazione lineare?

## 4
In ogni caso il modello che ho ottenuto in questo caso sembra buono. Anche se il test Anova da evidenza per affermare che il modello ottenuto è significativamente differente rispetto al modello che tiene in considerazione ogni variabile sceglierei questo modello visto che l'R-Squared è molto buono (anche quello Adjusted) e in questo caso il modello è molto più semplice rispetto a quello che tiene in considerazione ogni cosa.

## 5
Provare con natural cubic spline e controllare i risultati? Modelli ad effetti misti? come utilizzare i clustering in questo caso?
