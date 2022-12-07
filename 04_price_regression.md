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

## PROVO UN APPROCCIO DIVERSO 
Osservo i pairs delle variabili (PRICE, ACC, LENGTH, HEIGHT, RANGE, POWER, BATTERY_CAPACITY, CARGO_VOL, CONSUMPTION) colorando rispetto alla trazione e ai 3 clustering individuati. Cerco di indivudare delle _relazioni tra le singole variabili e la risposta_ (PRICE). 

Fitto modelli con una singola variabile e i vari clustering. Mi accorgo che la potenza della macchina è veramente importante per predire il prezzo. 
Utilizziamo la singola variabile per profare a fare una regressione nonparametrica tramite diverse tecniche (**smoothiing splines, cubic splines, natural cubic splines**). I risultati sono in ogni caso ottimi. 

A questo punto cerco di creare dei modelli che abbiano un senso per la nostra analisi (indipendentemente dall'R-squared che in ogni caso sembra molto elevato. _Sempre > 0.8_). Ho creato alcuni modelli aggiungendo alcune variabili e i custering che abbiamo creato e ho ottenuto dei buoni risultati. Notare anche che spesso sono molto importanti le interazioni!
Il modello che mi sembra molto interessante è:
 --> `PRICE ~ s(POWER, bs = "cr") + s(HEIGHT, bs = "cr") + s(I(POWER * HEIGHT), bs = "cr") + clustering.m2`

Questo modello mi sembra molto molto interessante perché è molto semplice e consente di predire in maniera molto accurata il prezzo di una macchina partendo semplicemente dalla potenza e dall'altezza (più la classificazione VAN vs CAR). E' utile un modello di questo tipo perché possiamo eseguire dei plot (QUINDI CERCARE DI TENERE UN MASSIMO DI 2 PREDITTORI, piuttosot includere le interazioni e i clustering (meglio se la classificazione VAN vs CAR)). 
A partire da qui avendo a disposizione anche i modelli per il RANGE e per il CONSUMPTION forse possiamo fare qualcosa di più interessante.
