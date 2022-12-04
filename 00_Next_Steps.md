1. [x] Eliminare info ridondanti (Luca). 
2. [x] Analisi outlier (caratterizzare gli outlier rispetto alle varie features: una certa unità è un outlier perché? Da scrivere) (Luca)
3. [ ] Decidere come trattare i punti problematici (leggere file descrittivo sull'analisi outlier --> macchina scarsa, supercar, veicoli che si caricano molto velocemente e furgoni)     
4. [ ] test non parametrici per studiare il comportamento delle popolazioni outlier vs non. Test nonparametrici per studiare la differenza tra le popolazioni identificate dal clustering o dai diversi tipi di trazione.
5. [x] clustering con hierarchical clustering (distanza "gower") per cercare di distinguere le popolazioni (Luca). 
5.1. [ ] analizzando il risultato del clustering mi è venuto in mente che si potrebbe usare permutational Manova (tecnica nonparametrica da confrontare con il clustering fatto con hclust e kmeans)
7. [ ] regressioni non lineari! (GAMS + altri modelli)
8. [ ] studio di Vehicle fuel equivalent e Consumptions

DOMANDE PER CAPPOZZO: 
1. Come trattare outliers? (io a priori non rimuoverei nessun punto, perchè sono macchine e non osservazioni sbagliate: vale la pena fare considerazioni su qualche variabile? ad esempio: CHARGE_SPEED che ha dei pesanti outlier si può pensare di rimuoverla? C'è un punto che ha accelerazione mega elevata: cosa ne facciamo?)
2. Come utilizzare al meglio i clustering che ho ottenuto? C'è un modo per utilizzarli? Magari nella regressione? Si può pensare di utrilizzare i famosi modelli ad effetti misti?
