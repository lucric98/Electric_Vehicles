1. [x] Eliminare info ridondanti (Luca). 
2. [x] Analisi outlier (caratterizzare gli outlier rispetto alle varie features: una certa unità è un outlier perché? Da scrivere) (Luca)
3. [x] Decidere come trattare i punti problematici (leggere file descrittivo sull'analisi outlier --> macchina scarsa, supercar, veicoli che si caricano molto velocemente e furgoni) -> teniamo tutte le macchine (Luca)  
4. [ ] test non parametrici per studiare il comportamento delle popolazioni outlier vs non. Test nonparametrici per studiare la differenza tra le popolazioni identificate dal clustering o dai diversi tipi di trazione.
5. [x] clustering con hierarchical clustering (distanza "gower") per cercare di distinguere le popolazioni (Luca). 
5.1. [x] analizzando il risultato del clustering mi è venuto in mente che si potrebbe usare permutational Manova (tecnica nonparametrica da confrontare con il clustering fatto con hclust e kmeans) (Luca)
7. [ ] regressioni non lineari! (GAMS) (Luca - Ale - Lorenzo). Cerchiamo di ottenere dei modelli per il Prezzo, Range e Consumption
7.1 [ ] Modelli ad effetti misti (questi sono utili per tenere conto dei clustering e dei vari gruppi). A mio parere possono essere più utili rispetto ai GAMS perché non possiamo tenere in considerazioni interazioni non lineari tra le covariate.
9. [ ] studio di Vehicle fuel equivalent e Consumptions: le due variabili sono una la combinazione lineare dell'altra!
10. [ ] 

DOMANDE PER CAPPOZZO: 
1. Come trattare outliers? (io a priori non rimuoverei nessun punto, perchè sono macchine e non osservazioni sbagliate: vale la pena fare considerazioni su qualche variabile? ad esempio: CHARGE_SPEED che ha dei pesanti outlier si può pensare di rimuoverla? C'è un punto che ha accelerazione mega elevata: cosa ne facciamo? In fase di modelling eliminiamo la variabile da tutti i modelli -> vedi ScatterplotMatrix: la curva viene )
2. Come utilizzare al meglio i clustering che ho ottenuto? C'è un modo per utilizzarli? Magari nella regressione? Si può pensare di utrilizzare i famosi modelli ad effetti misti?
3. Come legare tutta questa prima parte con il goal del nostro progetto (capire se le macchine elettriche sono veramente convenienti da un punto di vista economico per le persone)? In particolare come legarlo utilizzando tecniche non parametriche.
