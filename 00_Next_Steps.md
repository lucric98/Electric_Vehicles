# OBIETTIVO
Stimare il costo **COMPLETO** di una macchina elettrica per una famiglia. Un'analisi basilare richiederebbe di fare prezzo + km_auto x prezzo elettricità. Come stimiamo il prezzo dell'elettricità e i km percorsi da un'automobile? Cercare dati su internet.
Abbiamo modelli per predire prezzo e altre informazioni avendo solo caratteristiche fisiche della macchina. => Questo è molto importante perché una famiglia potrebbe partire dalle caratteristiche fisiche della macchina per capire il costo complessivo dell'automobile!
La regressione ci fornisce _intervalli di confidenza con la deviazione standard per quanto riguarda il prezzo_.
Possiamo aggiungere per step successivi altri costi (stimare queste predizioni tramite intervalli di conformal prediction ad esempio per il numero di kilometri e il costo dell'elettricità visto che sono due fattori importanti):
1. [ ] Stimare costo elettricità
2. [ ] Numero di kilometri percorsi da una macchina durante la sua vita
3. [ ] Capire come sommare tutte le varianze per ottenere l'intervallod i confidenza 5%-95%.
4. [ ] Raccogliere dati che servono! (QUESTO E' IL PRIMO DA PORTARE A TERMINE)
Step successivi => complicare il modello introducendo altri costi della macchina (tagliando/revisione stimati sempre tramite intervallo di conformal)

A) Mi piacerebbe usare la variabile Vehicle Fuel Equivalent in qualche modo. come la potremo usare?
B) Mi piacerebbe tenere in considerazione anche le variabili battery_capacity e charge_speed --> capire quale macchina da un punto di vista della batteria conviene acquistare (cercare dati sui pannelli fotovoltaici: quale potenza si riesce a ricavare dai pannelli, e quindi quale è la batteria che può essere ricaricata meglio?)

TRARRE CONCLUSIONI RIGUARDO TUTTO!
