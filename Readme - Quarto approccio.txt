## QUARTO APPROCCIO
Inizialmente, nel codice riguardante il quarto approccio, viene settata la directory e caricato il dataset "santa_cities.csv" 
che viene salvato in due dateset ulteriori ( d e d1) che saranno successivamente utilizzati ognuno per calcolare i due 
percorsi. 
Innanzitutto si sceglie di calcolare il primo percorso andando a trovare la città più in basso a sinistra nel piano cartesiano 
e  si crea un dataset vuoto nel quale verranno salvati i risultati per il primo percorso.
Il codice poi, presenta la funzione principale nella quale viene calcolata la matrice delle distanze alla quale verrà 
applicata la funzione TSP che utilizza il metodo 2-opt. Il fine è trovare il percorso ottimale identificando la città più 
vicinina alla precedente inserita. Terminata questa fase, si procede a riprodurre tutto il processo per il calcolo della 
seconda strada sul dataset d1  trovando,però, la città di partenza come quella più vicina ai valori nell'angolo in alto a 
destra del piano cartesiano. Durante questo procedimento, il codice presenta anche una parte in cui viene effettuato 
parallelamente un controllo grazie al quale l'algoritmo evita di identificare archi tra nodi uguali od opposti presenti in 
entrambi i percorsi.
Infine, una volta identificati i due percorsi, viene applicato ulteriormente l'algoritmo di 2-opt per ottimizzare archi 
inefficienti. L'output finale del codice è 
- risultato
-risultato1
che identificano i due percorsi finali e vengono salvati nel documento "percorso_nn_opt_nocluster.csv".
