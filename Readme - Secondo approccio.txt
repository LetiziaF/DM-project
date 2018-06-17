## SECONDO APPROCCIO
Inizialmente, nel codice riguardante il secondo approccio, viene settata la directory e caricato il dataset "santa_cities.csv". 
Il dataset viene sottoposto ad un procedimento di k-means al fine di individuare la suddivisione dei nodi presenti in esso in 
50 cluster e trovare per ognuno di essi i centroidi corrispondenti. 
Dopo aver calcolato la matrice delle distanze tra i centroidi, viene applicato l'algoritmo Genetico dal quale si ottengono due 
cluster estremi.
Partendo dal cluster estremo più vicino all'origine, si individua il suo centroide e si esegue un algoritmo di Nearest Neighbor
tra tutti i suoi punti, e si individua il percorso totale passando tra i cluster in modo tale da minimizzare la distanza tra 
un nodo A e un nodo B, esterno al percorso. Si applica contemporaneamente un algoritmo 2-opt che ottimizzi parallelamente tutti gli archi
inefficienti. Una volta inclusi tutti i nodi nel percorso, si trova la prima soluzione.
Tale procedimento viene effettuato anche per il secondo percorso che utilizza come cluster di partenza, il cluster estremo individuato 
grazie al Genetic Algorithm e non usato per il primo percorso.
infine, nel codice, viene inserito un passaggio obbligatorio di controllo degli archi, il quale fa sì che i collegamenti tra i nodi 
nei due percorsi non siano ripetuti in maniera uguale od opposta. 
Infine, i risultati dei due percorsi vengono salvati nell'output: 
- risultato 
-risultato2
e vengono inseriti nel documento "percorso_nngenetic_pulito.csv".
