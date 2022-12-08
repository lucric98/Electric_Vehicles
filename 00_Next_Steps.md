1. [x] Eliminare info ridondanti (Luca). 
2. [x] Analisi outlier (caratterizzare gli outlier rispetto alle varie features: una certa unità è un outlier perché? Da scrivere) (Luca)
3. [x] Decidere come trattare i punti problematici (leggere file descrittivo sull'analisi outlier --> macchina scarsa, supercar, veicoli che si caricano molto velocemente e furgoni) -> teniamo tutte le macchine (Luca)  
4. [ ] test non parametrici per studiare il comportamento delle popolazioni outlier vs non. Test nonparametrici per studiare la differenza tra le popolazioni identificate dal clustering o dai diversi tipi di trazione.
5. [x] clustering con hierarchical clustering (distanza "gower") per cercare di distinguere le popolazioni (Luca). 
5.1. [x] analizzando il risultato del clustering mi è venuto in mente che si potrebbe usare permutational Manova (tecnica nonparametrica da confrontare con il clustering fatto con hclust e kmeans) (Luca)
7. [ ] regressioni non lineari! (GAMS) (Luca - Ale - Lorenzo). Cerchiamo di ottenere dei modelli per il Prezzo, Range e Consumption. Modelli devono essere il più semplice possibile (importante non overfittare i dati)
7.1 [ ] Modelli ad effetti misti (questi sono utili per tenere conto dei clustering e dei vari gruppi). A mio parere possono essere più utili rispetto ai GAMS perché non possiamo tenere in considerazioni interazioni non lineari tra le covariate. (BOCCIATO)
9. [x] studio di Vehicle fuel equivalent e Consumptions: le due variabili sono una la combinazione lineare dell'altra!
10. [ ] Ricerca di dati riguardanti macchine non elettriche per effettuare i confronti riguardo alle tre variabili a cui siamo interessati (PRICE, RANGE, CONSUMPTION). Questi confronti verrano fatti sfruttando sicuramente i modelli che abbiamo ottenuto, ma è possibile applicare anche qualche tecnica non parametrica tipo test permutazionali (sulla distribuzione/medie, etc di queste variabili)?
11. [x] Aggregare macchine elettriche per modello
12. [ ] Stimare un indice (indipendente dalla popolarità del brand, etc) che mi permetta di confrontare il numero di veicoli elettrici venduti (in Germania) con il numero di veicoli fuel. A partire da questo possiamo confrontare le variabili.
13. [ ] Presentazione (ci saranno da fare sicuramente grafici):
14.   SLIDE 1: Titolo e gruppo. (foto di noi, titolo)
15.   SLIDE 2: Presentazione: che dati abbiamo a disposizione (modifica della slide per spiegare i dati a disposizione)
16.   SLIDE 2.1: Obiettivo: qual è l'obiettivo della nostra analisi?
17.   SLIDE 3: analisi correlazione (mostrare grafico delle variabili linearmente correlate che abbiamo tolto: 1. tutti i range sono linearmente correlati e ne teniamo uno solo, 2. le caratteristiche fisiche della macchine --> **pairs con ggGally!**). Analisi outlier: supercar/macchina scarsa/furgoni/charge speed. (bisogna fare dei **boxplot carini**). NON eliminiamo a priori nessuna riga del dataset perché si tratta di macchine e non di errori di misura etc.
18.   SLIDE 4: Notando che è possibile suddividre i veicoli elettrici in Van vs Car tentiamo di applicare tecniche di Hierarchial Clustering per capire se è possibile separare le due categorie. Otteniamo 4 categorizzazioni dei nostri dati: 1) Van vs Car (2 clusters, Mcquitty linkage) 2) ... (**plot dei cluster**)
19.   SLIDE 5: Modelli nonlineari per le variabili che siamo interessati a studiare (**scrivere formule**). Cerchiamo dei modelli semplici
20.   SLIDE 6: Next Steps: ricerca di dati per macchine non elettriche per effettuare confronti + stima di un indice di confronto (a partire dai modelli venduti).

DOMANDE PER CAPPOZZO: 
1. Come trattare outliers? (io a priori non rimuoverei nessun punto, perchè sono macchine e non osservazioni sbagliate: vale la pena fare considerazioni su qualche variabile? ad esempio: CHARGE_SPEED che ha dei pesanti outlier si può pensare di rimuoverla? C'è un punto che ha accelerazione mega elevata: cosa ne facciamo? In fase di modelling eliminiamo la variabile da tutti i modelli -> vedi ScatterplotMatrix: la curva viene )
2. Come utilizzare al meglio i clustering che ho ottenuto? C'è un modo per utilizzarli? Magari nella regressione? Si può pensare di utrilizzare i famosi modelli ad effetti misti?
3. Come legare tutta questa prima parte con il goal del nostro progetto (capire se le macchine elettriche sono veramente convenienti da un punto di vista economico per le persone)? In particolare come legarlo utilizzando tecniche non parametriche.
