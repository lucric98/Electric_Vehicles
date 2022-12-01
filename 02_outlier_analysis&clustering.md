# OUTLIER ANALYSIS
Nel file 01_analisi_outlier.r si trova una prima analisi degli outlier in cui ho cercato di analizzare prima gli outlier delle singole variabili e poi ho provato ad usare dei bagplot.
Ho suddiviso le variabili per categoria (vedi presentazione per capire come ho suddiviso le variabili).

## ANALISI VARIABILI SINGOLE
- Caratteristiche fisiche:
	- Come già evidenziato i furgoni sono outlier rispetto alla variabile height (>1800? vedi le prime righe del file di R per questa caratterizzazione.) --> vale la pena effettuare qualche test riguardo questo
	- Anche per quanto riguarda la categoria Payload/Cargo Volume ci sono molti outlier (non tutti questi sono furgoni! Sono semplicemente macchine che possono portare un carico molto elevato, come le trattiamo?)
	- Forse varrebbe la pena utilizzare la variabile GVWR che che esprime informazioni “simili” e non presenta outlier così pesanti? (problema di GVWR: è molto correlata con la lunghezza, molto più rispetto a Payload e Cargo Volume)
	- Abbiamo alcuni outlier anche per Length e Width (500 perché è molto piccola)
- Per quanto riguarda le caratteristiche di range c’è un solo outlier ed è una supercar.
	- Sono tutte altamente correlate: alla fine ne terremo una sola, che è Electric.Range
- Caratteristiche di batteria.
	- Non sono altamente correlate queste variabili quindi potremmo anche pensare di tenerle tutte e 3, anche se Charge.Speed ha degli outlier piuttosto pesanti (quindi forse se dobbiamo scegliere conviene levare questa).
- Prezzo e consumi.
	- Il prezzo ha molti outlier (Supercar)
	- Vehicle fuel equivalent e Consumption sono altamente correlate (in fase di analisi una va tolta, probabilmente ha senso togliere Vehicle Fuel Equivalent)
Secondo me a priori non ha senso “eliminare” nessun outlier, semplicemtne bisogna tenere in considerazione che le macchine hanno caratteristiche diverse tra di loro. L’unica cosa che si potrebbe provare a fare è eliminare i furgoni (outlier in alcune caratteristiche fisiche e di batteria) o le supercar (outlier in poche caratteristiche fisiche e nel prezzo) e vedere come varia la situazione.
Oss: c’è una macchine che però è outlier in molte categorie(macchina piuttosto scarsa dal punto di vista delle prestazioni --> https://ev-database.org/car/1705/Dacia-Spring-Electric potremmo pensare di rimuovere lei?)

## ANALISI BAGPLOT/PAIRS
- Caratteristiche fisiche:
	- Teniamo: ACCELERATION, LENGTH, POWER, HEIGHT, CARGO_VOLUME, MAX_PAYLOAD. (ne possiamo togliere una tra le ultime due?)
- Range
	- Teniamo: ELECTRIC_RANGE
- Caratteristiche di batteria.
	- Le terrei tutte e 3 a questo punto dell’analisi (anche se come osservato in precedenza Charge Speed presenta outlier piuttosto pesanti). CHARGE_SPEED, FASTCHARGE_SPEED, BATTERY_CAPACITY.
- Prezzo e consumi.
	- Teniamo: PRICE, CONSUMPTION. (Vehicle fuel equivalent ci servirà poi più avanti per fare il confronto)
Alla fine ho fatto ho una bagplot con tutte queste caratteristiche: come evidenziato l’unica che mi sembra dare problemi rilevanti è CHARGE_SPEED per cui ci sono pochi outlier ma piuttosto pesanti. https://ev-database.org/car/1705/Dacia-Spring-Electric la teniamo? (`forse varrebbe chiedere aiuto a cappozzo per questa`).

## CLUSTERING
Ho applicato Hierarchical clustering sul dataset ridotto (ho tenuto solo le variabili in MAIUSCOLO a cui ho aggiunto le 3 variabili categoriche: SEATS, DRIVE, CHARGE_POWER). Per ottenere clustering sensato ho usato Hierarchical Clustering (_distance="gower"_ -> una distanza che permette di trattare variabili categoriche e continue assieme, _average linkage_, _mcquitty linkage_, _ward linkage_, _k=2_, _k=3_). 
* **_K = 2_**. Il miglior risultato sembra essere dato dall'**average/mcquitty linkage**: è in grado distinguere molto bene la differenza tra furgoni e macchine (a parte qualche punto, però sembra clusterizzare molto bene questo aspetto)
* **_K = 3_**_ `Vorrei far vedere questo risultato a Cappozzo per capire se sia meglio tenere 2 o 3 clusters`. A me sembra che il risultato migliore in questa situazione sia dato dal **Mcquitty** (o **Ward**?), che ci permette di distinguere le macchine in 3 categorie (vedi accelerazione, però non capisco se sia utilizzabile per davvero).
* Un ultimo modo per clusterizzare ii dati consiste nel suddividere i dati secondo il tipoo di trazione (Awd, front, Rear).

Ad ogni modo i risultati i sembrano più che accettabili, anzi direi che sono buoni.
Se tolgo le variabili categoriche e cerco di appplicare il clustering ottengo risultati poco sensati e non mi sembra il caso di perdere troppo tempo per cercare di applicare il k-means (è molto complicato applicarlo quando ci sono variabili categoriche). Quindi visto che ho ottenuto risultati più o meno soddisfacenti adesso bisogna confrontare questo tipo di analisi con una **permutational ANOVA** --> tecnica del corso!

## PERMUTATIONAL MANOVA/ANOVA
Ho applicato il permutational MANOVA per le variabili scritte in MAIUSCOLO (togliendo le variabili categoriche) nei seguenti gruppi:
* **2 clusters - Mcquitty linkage** (Car vs Van)
* **3 clusters - Ward linkage**
* **3 clusters - Mcquitty linkage**
* **Trazione (Front, Rear, AWD)**
In tutti i casi il permutational p-value è 0, questo significa che esiste una differenza significativa nelle medie dei vari gruppi
Dopo di chè ho applicato permutational ANOVA sulle singole caratteristiche per ciascuno dei 4 clustering. Anche in questo caso molto spesso si osservano differenze nelle medie dei vari gruppi (non di tutti, però della maggior parte. i risultati sono scritti sotto forma di commenti nel file di R).
