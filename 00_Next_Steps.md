# OBIETTIVO
Stimare il costo **COMPLETO** di una macchina elettrica per una famiglia. Un'analisi basilare richiederebbe di fare prezzo + km_auto x prezzo elettricità. Come stimiamo il prezzo dell'elettricità e i km percorsi da un'automobile? Cercare dati su internet.
Abbiamo modelli per predire prezzo e altre informazioni avendo solo caratteristiche fisiche della macchina. => Questo è molto importante perché una famiglia potrebbe partire dalle caratteristiche fisiche della macchina per capire il costo complessivo dell'automobile!
La regressione ci fornisce _intervalli di confidenza con la deviazione standard per quanto riguarda il prezzo_.
Possiamo aggiungere per step successivi altri costi (stimare queste predizioni tramite intervalli di conformal prediction ad esempio per il numero di kilometri e il costo dell'elettricità visto che sono due fattori importanti):
1. [x] Stimare costo RICARICA (mentre per le automobili a combustibile fossile il costo di ricarica è semplicemente il costo del combustibile per le auto elettriche il discorso è molto più complesso, perché non possiamo approssimarlo con il costo dell'elettricità!) -> Troppo difficile stimare direttamente il costo di ricarica (non è sufficiente stimare il prezzo dell'elettricità, ci sono molti fattori che concorrono con il prezzo effettivo di ricarica di un veicolo elettrico). Utilizziamo il risultato dell'articolo trovato qui: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9458728/#CR7. In questo articolo vengono identificati 4 diversi profili di LCOC (LEVELIZED COST OF CHARGING) (ciascuno dei quali identifica un possibile e diverso tipo di costo di ricarica: Esempio utente che ricarica la macchina prevalentemente a casa, senza e con l'utilizzo di pannelli solari). **Problema: i dati relativi al costo dell'elettricità si riferiscono al 2019, prima della pandemia e della guerra tra Ucraina e Russia, che ha determinato un aumento significativo del costo dell'elettricità, sia per le aziende che per i privati. (è uno dei limiti del nostro approccio, ma a cui non è possibile trovare soluzione vista la mancanza di dati)**
2. [ ] Numero di kilometri percorsi da una macchina durante la sua vita (macchina con vita breve, macchina con vita media e macchiina con vita lunga)
3. [ ] Raccogliere dati che servono! (QUESTO E' IL PRIMO DA PORTARE A TERMINE)
4. [ ] In questo modo possiamo calcolare delle regioni di conformal prediction (usando PRICE e CONSUMPTION per ogni veicolo, possiamo identificare delle regioni di conformal prediction per il prezzo totale di un'automobile qualsiasi, usando i diversi profili di LCOC e il numero di kilometri percorsi da un'automobile durante la sua vita). Siccome per ogni profilo di LCOC e numero di kilometri di vita di un veicolo elettrico avremo a disposizione una deviazione standard, le regioni di confidenza che possiamo ricavare sono molte. Dobbiamo concentrarci su alcune! 
5. [ ] OBIETTIVO --> ottenere una sorta di prezzo al kilometro per ogni macchina elettrica, di modo che capiamo quale ci convenga acquistare. 
6. [ ] Analisi Residui per case automobilistiche (valutare rapporto qualità/prezzo) --> consiglio casa!
7. [ ] Scrivere Report
8. [ ] Fare grafici
9. [ ] Presentazione + discorso.


- Mi piacerebbe usare la variabile Vehicle Fuel Equivalent in qualche modo. come la potremo usare?
-  Mi piacerebbe tenere in considerazione anche le variabili battery_capacity e charge_speed --> capire quale macchina da un punto di vista della batteria conviene acquistare (cercare dati sui pannelli fotovoltaici: quale potenza si riesce a ricavare dai pannelli, e quindi quale è la batteria che può essere ricaricata meglio?)

TRARRE CONCLUSIONI RIGUARDO TUTTO!
