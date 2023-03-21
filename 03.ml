
let verifica(h,m)=
  (h>=0 && h<24) && (m>=0 && m<60) 
 
let somma_minuti (m1,m2)=
  let mtot=m1+m2
  in (mtot/60, mtot mod 60)

let somma_ore(h1,m1)(h2,m2) =
  if(verifica(h1,m1) && verifica(h2,m2)) then 
    let (h3,m3) = somma_minuti(m1,m2) in
    ((h1+h2+h3)mod 24, m3)
  else 
    failwith "Errore" 

(**************************ESERCIZIO 2*****************************)
(*read_max: unit->int, la funzione read max legge da tastiera una
sequenza di numeri interi (anche negativi), separati da Enter e terminata dalla
stringa vuota o da qualsiasi stringa che non rappresenti un intero.Let rec aux 
è una funziona ausilia
ria che contiente un accumulatore che viene inizializzata
con il primo valore letto (read_int). Inizializzo una variabile n
che sarà l'intero che confronterò. Richiamo infine aux e faccio il massimo 
tra ciò che è nell'accumulatore e n , altrimenti alzo un'eccezione*)


let read_max () =
 let rec aux acc = 
   try 
    let n = read_int() (*Verifico che n sia un interoche poi confronto*)
    in aux ( max acc n) 
   with _ -> acc 
 in try aux (read_int ()) (*Inizializzazione*) 
    with _ ->  failwith "Errore"

(*************************ESERCIZIO 2b******************************)

(* read_max_min: unit->int*int *) 

let  read_max_min () =
  let rec aux a b  =    (*a=massimo, b=minimo*)
    try 
      let n = read_int() in
      aux ( max a n) ( min b n)
    with _-> (a,b)
   in 
   try let k = read_int() in 
       aux k k                (*Inizializzazione*)
    with _-> failwith "Errore"
(*************ESERCIZIO 2c************)

(*tutti_minori: int->bool*)
(*tutti_minori,preso un intero n, dopo che legge gli interi da tastiera dice
se questi sono tutti minori di n. Non termina finchè non viene immessa 
la stringa vuota*)

let rec tutti_minori n = 
 try let k = read_int()
  in tutti_minori n && k<n
 with _ -> true
    
(************ESERCIZIO 2d***************)

let rec occorre n = 
  try let k = read_int()
  in occorre n || k=n
  with _ -> false
(************ESERCIZIO 2e***************)
let num_di_stringhe () =
  let rec aux a = 
    if read_line() = ";;"  then a 
    else aux (a+1)
 in aux 0

(************ESERCIZIO 2f***************)

(* stringa_max: unit -> string *)
(* aux implementa un ciclo: ha due parametri, la stringa di lunghezza
     massima tra quelle lette fino a quel momento, e la sua lunghezza (per
     evitare di calcolarla piu' volte *)
(* aux :  string -> int -> string *)

let stringa_max () =
  let rec aux smax len =
      let nuova = read_line()
      in if nuova = ";;" then smax
         else let nuovalen = String.length nuova 
              in if len < nuovalen 
                 then aux nuova nuovalen
                 else aux smax len
   in aux "" 0
(*******ESERCIZIO 3a*******)
(*sumbetween: int->int->int*)
(*sumbetween è una funzione che implementa la somma degli interi compresi
tra n e m che sono i parametri*)

let sumbetween n m =
  let rec aux tot x y =
   if x > y then tot 
   else aux (tot+x) (x+1) y 
  in aux 0 n m     

(*****ESERCIZIO 3b******)

let sumto  n =
  let rec aux tot x y =
  if x > y then tot 
  else aux (tot+x) (x+1) y 
  in aux 0 0 n 


(*****ESERCIZIO 3c*****)

let rec  power n k = 
 if k = 0 then 1
 else n * power n (k-1)

(******ESERCIZIO 3d******)

let rec fib  = function 
   0->0
  |1->1
  |n->fib(n-1)+fib(n-2)

(****ESERCIZIO 3e******)

(* maxstring: string -> char *)
(*  aux : char -> int -> char *)
(* aux implementa il ciclo di scansione della stringa: 
   maxchar e' il massimo carattere
   incontrato fino a questo punto, i e' la posizione del carattere 
   da considerare *)

let maxstring s =
  let rec aux maxchar i =
    try
      aux (max maxchar s.[i]) (i+1)
    with _ -> maxchar (*stringa terminata*)
  in                    (* consideriamo anche il caso della stringa vuota *)
  try aux s.[0] 1

                 (* inizializzazione del ciclo con il carattere in posizione 0,
                  iniziando dal successivo *)
  with _ -> failwith "Stringa vuota"
   (* questa eccezione sara' sollevata solo nel caso in cui s="",
      per evitare che sia sollevata l'eccezione predefinita
      Invalid_argument "index out of bounds" *)

