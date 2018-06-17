## TERZO APPROCCIO
Inizialmente, nel codice riguardante il terzo approccio, viene settata la directory e caricato il dataset "cluster_kmeans.csv".
Tale file è l'output ottenuto a seguito di un procedimento di k-means applicato al dataset "santa_cities.csv", intrapreso per 
poter individuare la suddivisione dei nodi presenti in esso in 50 cluster. 
Vengono creati due dataframe vuoti (solution e solution2) nei quali verranno inseriti i risultati finali. 
Come primo passaggio, vengono calcolati i centroidi di ogni cluster e scelti i due cluster iniziali di ogni percorso.
Tale scelta viene fatta individuando:
- per il primo percorso, il cluster con il centroide più vicino all'origine del paino cartesiano
- per il secondo percorso, il cluster con il centroide più vicino all'angolo in alto a destra del piano cartesiano.
Successivamente, nel codice si trova la funzione principale nella quale viene calcolato il percorso ottimale all'interno del
singolo cluster secondo una logica di Nearest Neighbor e di 2-opt al fine di ottimizzare il percorso interno. 
Una volta individuato il nodo finale del percorso all'interno del cluster preso in considerazione, l'algoritmo calcola la
distanza tra esso e tutti i punti medi dei cluster non ancora inseriti nel percorso totale. Una volta trovato il nodo con il
quale si minimizza l'arco di collegamento, l'algoritmo seleziona il corrispondente cluster ed effettua il processo di calcolo
del percorso interno, come descritto in precedenza. Tale procedimento viene eseguito per tutti i cluster presenti nel dataset, 
cosicchè da ottenere il primo percorso totale. 
Il processo viene eseguito allo stesso modo per il secondo percorso totale e contemporaneamente viene effettuato un controllo
su tutte le combinazioni dei nodi tramite l'assegnazione di pesi a tutte quelle combinazioni già avvenute nel primo percorso 
che non possono essere ripetute. Il codice è scritto in modo tale che l'algoritmo non scelga tutte quelle combinazioni che 
hanno una distanza elevatissima e, se la combinazione è già avvenuta, non sarà scelta poichè avrà associato un peso molto 
alto.
Infine, una volta identificati i due percorsi, i risultati vengono salvati nell'output: 
- ris
-ris1
e vengono salvati nel documento "50percorso_nnotp_kmeans_progressivo.csv".
