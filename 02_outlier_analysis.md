# OUTLIER ANALYSIS
Nel file 01_analisi_outlier.r si trova una prima analisi degli outlier in cui ho cercato di analizzare prima gli outlier delle singole variabili e poi ho provato ad usare dei bagplot.
Ho suddiviso le variabili per categoria (vedi presentazione per capire come ho suddiviso le variabili).

**ANALISI VARIABILI SINGOLE:**
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
Secondo me a priori non ha senso “eliminare” nessun outlier, semplicemtne bisogna teenre in considerazione che le macchine hanno caratteristiche diverse tra di loro. L’unica cosa che si potrebbe provare a fare è eliminare i furgoni (outlier in alcune caratteristiche fisiche e di batteria) o le supercar (outlier in poche caratteristiche fisiche e nel prezzo) e vedere come varia la situazione.
Oss: c’è una macchine che però è outlier in molte categorie(macchina piuttosto scarsa dal punto di vista delle prestazioni --> https://ev-database.org/car/1705/Dacia-Spring-Electric potremmo pensare di rimuovere lei?)

**ANALISI BAGPLOT/PAIRS:**
- Caratteristiche fisiche:
	- Teniamo: ACCELERATION, LENGTH, POWER, HEIGHT, CARGO_VOLUME, MAX_PAYLOAD. (ne possiamo togliere una tra le ultime due?)
- Range
	- Teniamo: ELECTRIC_RANGE
- Caratteristiche di batteria.
	- Le terrei tutte e 3 a questo punto dell’analisi (anche se come osservato in precedenza Charge Speed presenta outlier piuttosto pesanti). CHARGE_SPEED, FASTCHARGE_SPEED, BATTERY_CAPACITY.
- Prezzo e consumi.
	- Teniamo: PRICE, CONSUMPTION. (Vehicle fuel equivalent ci servirà poi più avanti per fare il confronto)
Alla fine ho fatto ho una bagplot con tutte queste caratteristiche: come evidenziato l’unica che mi sembra dare problemi rilevanti è CHARGE_SPEED per cui ci sono pochi outlier ma piuttosto pesanti. https://ev-database.org/car/1705/Dacia-Spring-Electric la teniamo? (forse varrebbe chiedere aiuto a cappozzo per questa).
