# OBIETTIVO
Stimare il costo **COMPLETO** di una macchina elettrica per una famiglia. Un'analisi basilare richiederebbe di fare prezzo + km_auto x prezzo elettricità. Come stimiamo il prezzo dell'elettricità e i km percorsi da un'automobile? Cercare dati su internet.
Abbiamo modelli per predire prezzo e altre informazioni avendo solo caratteristiche fisiche della macchina. => Questo è molto importante perché una famiglia potrebbe partire dalle caratteristiche fisiche della macchina per capire il costo complessivo dell'automobile!
La regressione ci fornisce _intervalli di confidenza con la deviazione standard per quanto riguarda il prezzo_.
Possiamo aggiungere per step successivi altri costi (stimare queste predizioni tramite intervalli di conformal prediction ad esempio per il numero di kilometri e il costo dell'elettricità visto che sono due fattori importanti):
1. [x] Stimare costo RICARICA (mentre per le automobili a combustibile fossile il costo di ricarica è semplicemente il costo del combustibile per le auto elettriche il discorso è molto più complesso, perché non possiamo approssimarlo con il costo dell'elettricità!) -> Troppo difficile stimare direttamente il costo di ricarica (non è sufficiente stimare il prezzo dell'elettricità, ci sono molti fattori che concorrono con il prezzo effettivo di ricarica di un veicolo elettrico). Utilizziamo il risultato dell'articolo trovato qui: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9458728/#CR7. In questo articolo vengono identificati 4 diversi profili di LCOC (LEVELIZED COST OF CHARGING) (ciascuno dei quali identifica un possibile e diverso tipo di costo di ricarica: Esempio utente che ricarica la macchina prevalentemente a casa, senza e con l'utilizzo di pannelli solari). **Problema: i dati relativi al costo dell'elettricità si riferiscono al 2019, prima della pandemia e della guerra tra Ucraina e Russia, che ha determinato un aumento significativo del costo dell'elettricità, sia per le aziende che per i privati. (è uno dei limiti del nostro approccio, ma a cui non è possibile trovare soluzione vista la mancanza di dati)**
2. [ ] Numero di kilometri percorsi da una macchina durante la sua vita (macchina con vita breve, macchina con vita media e macchiina con vita lunga)
3. [ ] Raccogliere dati che servono! (QUESTO E' IL PRIMO DA PORTARE A TERMINE)
4. [ ] In questo modo possiamo calcolare delle regioni di conformal prediction (usando PRICE e CONSUMPTION per ogni veicolo, possiamo identificare delle regioni di conformal prediction per il prezzo totale di un'automobile qualsiasi, usando i diversi profili di LCOC e il numero di kilometri percorsi da un'automobile durante la sua vita). Siccome per ogni profilo di LCOC e numero di kilometri di vita di un veicolo elettrico avremo a disposizione una deviazione standard, le regioni di confidenza che possiamo ricavare sono molte. Dobbiamo concentrarci su alcune! 
6. [ ] Analisi Residui per case automobilistiche (valutare rapporto qualità/prezzo) --> consiglio casa!
7. [ ] Scrivere Report
8. [ ] Fare grafici
9. [ ] Presentazione + discorso.

## TOTAL_PRICE = PRICE + CONSUMPTION\*KM_LIFE*LCOC

 - PRICE <- Obtained thanks to Conformal Prediction thanks to our model
 - CONSUMPTION <- Obtained thanks to Conformal Prediction thanks to our model
 - KM_LIFE <- Life of the electric vehicle battery expressed in kilometers. Il ragionamento che facciamo per ottenere questo parametro è il seguente: abbiamo utilizzato la legge di decadimento temporale per la batteria di un veicolo elettrico accoppiata con i dati di decadimento finora osservati in alcune automobili (manually collected). Uno volta determinato il decadimento temporale (in anni) abbiamo definito 3 profili di utilizzo (in base al numero di kilometri che vengono percorsi da un'automobile in un determinato anno, visto che normalmente un'automobile non viene utilizzata per milioni di kilometri in un determinato anno)
 - LCOC <- Levelized costs of charging . Questo parametro si basa sul risultato di un articolo (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9458728/#CR7) e si basa sul fatto che per stimare il costo di ricarica di un veicolo elettrico non è sufficiente stimare il costo dell'elettricità perché ci sono molti fattori che entrano in gioco in questa stima. Quindi ci rifacciamo ad un'analisi già effettuata che ci garantisce 4 profili di costo di ricarica per la macchina elettrica in germania.


- Mi piacerebbe usare la variabile Vehicle Fuel Equivalent in qualche modo. come la potremo usare?
-  Mi piacerebbe tenere in considerazione anche le variabili battery_capacity e charge_speed --> capire quale macchina da un punto di vista della batteria conviene acquistare (cercare dati sui pannelli fotovoltaici: quale potenza si riesce a ricavare dai pannelli, e quindi quale è la batteria che può essere ricaricata meglio?)

TRARRE CONCLUSIONI RIGUARDO TUTTO!
