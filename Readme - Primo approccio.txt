## PRIMO APPROCCIO
Nel codice seguente viene dapprima settata la directory di lavoro e caricato il dataset "santa_cities.csv" sul quale si 
applica successivamente una tecnica di k-means che assegna tutti i nodi del dataset ad un numero di cluster predefinito pari a 
300. Vengono, poi, creati quattro dataframe nei quali si andrà a salvare i risultati.
Per ogni cluster creato, applico la funzione GAfit per trovare una soluzione per ognuno di essi e salvo i risultati in un 
dataframe denominato percorsi; contemporaneamente trovo i nodi eastremi di ogni cluster e li salvo nel dataframe estremi. 
Successivamente assegno dei pesi alle combinazioni già avvenute per far sì che queste non siano scelte ed inserite nel secondo 
percorso. Calcolo il secondo  percorso seguendo il procedimento del primo e l'algoritmo genetico.
Nel codice si setta la distanza tra tutti i nodi di ogni cluster pari a zero per poter poi trovare il percorso ottimale tra i 
cluster, tramite la funzione GAfit, senza separare i nodi appartenenti al singolo cluster, utilizzando gli estremi di ognuno 
per trovare il percorso totale ottimale assemblando tutte le soluzioni parziali.
Si effettua tale processo anche per la seconda soluzione trovata in precedenza, impostando contemporaneamente anche un 
controllo: si setta la distanza tra combinazione già avvenute ad infinito in modo tale che l'algoritmo non le possa inserire 
mai nella soluzione finale.
I dataset finali degli output dei due percorsi sono:
- ris 
- ris1
