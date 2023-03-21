type expr =
     Int of int
  | Var of string 
  | Sum of expr * expr
  | Diff of expr * expr 
  | Mult of expr * expr
  | Div of expr * expr

(*subexpr: expr -> expr -> bool che, date due
espressioni aritmetiche E1 e E2 determini se E2 è una sottoespressione di
E1.*) 

let rec subexpr e1 e2 = 
   e1=e2 ||
   match e1 with 
   Sum(x,y) | Diff(x,y) | Mult(x,y) | Div(x,y)
   -> subexpr x e2 || subexpr y e2 
|_ -> false 
   
(*******************************)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree


let tree = Tr("A",
                 Tr("H"
                   ,Tr("X"
                      ,Tr("Y",Empty,Empty)
                      , Empty) 
                   ,Empty
                   )
                  , Tr("O"
                      ,Tr("I" 
                         ,Empty
                         ,Tr("M"
                         ,Empty
                         ,Tr("T",Empty,Empty)
                            )
                         )
                      , Empty
                      )
             ) 


let rec reflect = function
    Empty -> Empty
  | Tr(x,t1,t2) -> Tr(x,reflect t2,reflect t1)

(********************************************************)

let tr = Tr(1
       ,Tr(2
         ,Tr(4
           ,Tr(8,Empty,Empty)
           ,Tr(9,Empty,Empty)
           )
         ,Tr(5
           ,Tr(10,Empty,Empty)
           ,Tr(11,Empty,Empty)
           )
         )
       ,Tr(3
         ,Tr(6
           ,Tr(12,Empty,Empty)
           ,Tr(13,Empty,Empty)
           )
         ,Tr(7
           ,Tr(14,Empty,Empty)
           ,Tr(15
               ,Tr(16
                   ,Tr(17,Empty,Empty),Empty),Empty)
           )
         )
       );;



(*fulltree:int -> int tree  La funzione, applicata a un intero n, riporta 
un albero binario completo di altezza n, con i nodi etichettati da interi
come segue: la radice è etichettata da 1, i figli di un nodo etichettato da k
sono etichettati da 2k e 2k + 1 *)

let fulltree n =   
  let rec aux k n =
    if n=0 then Empty 
    else Tr(k,aux(2*k) (n-1), aux (2*k+1) (n-1)) 
  in aux 1 n 
   

(********2c********)

(*balenced: 'a tree -> bool, determina se un albero è bilanciato*)

(*altezza: 'a tree -> int , calcola il percorso più lungo dalla radice alle
foglie che corrisponde all'altezza dell'albero binario. *)
let rec altezza = function 
     Empty -> 0 
  |Tr(x,Empty,Empty) -> 1 
  |Tr(x,left,right) -> 1 + (max (altezza left) (altezza right))  

 let rec balanced = function 
     Empty -> true 
  |Tr(_,t1,t2) -> balanced t1 && balanced t2
              && abs(altezza t1 - altezza t2) <=1
(******************************************)
let tr = Tr(1
       ,Tr(2
         ,Tr(4
           ,Tr(8,Empty,Empty)
           ,Tr(9,Empty,Empty)
           )
         ,Tr(5
           ,Tr(10,Empty,Empty)
           ,Tr(11,Empty,Empty)
           )
         )
       ,Tr(3
         ,Tr(6
           ,Tr(12,Empty,Empty)
           ,Tr(13,Empty,Empty)
           )
         ,Tr(7
           ,Tr(14,Empty,Empty)
           ,Tr(15,Empty,Empty)      
           )
         )
       );;

let tr11 = Tr("A1"
       ,Tr("A2"
         ,Tr("B"
           ,Tr("B",Empty,Empty)
           ,Tr("A6",Empty,Empty)
           )
         ,Tr("A4"
           ,Tr("A7",Empty,Empty)
           ,Tr("B",Empty,Empty)
           )
         )
       ,Tr("A3"
         ,Tr("A5"
           ,Tr("A8",Empty,Empty)
           ,Tr("B",Empty,Empty)
           )
         ,Tr("B"
           ,Tr("B",Empty,Empty)
           ,Tr("B",Empty,Empty)      
           )
         )
       );;


(*****2d****)
let rec preordine = function 
    Empty -> [] 
  |Tr(x,Empty,Empty) -> [x]
  |Tr(x,sx,dx) -> x::(preordine sx) @ (preordine dx)  

(*postordine: 'a tree -> 'a list*)

let rec postordine = function 
    Empty -> [] 
  |Tr(x,t1,t2) -> (postordine t1) @ (postordine t2) @ [x]   

let rec simmetrica = function 
    Empty -> [] 
  |Tr(x,t1,t2) -> (simmetrica t1) @ [x] @ (simmetrica t2) 

(**********************************)

(***2e***)
(*balpreorder e balinorder, entrambe di tipo 1a list -> 'a tree. Data una 
lista lst, costruiscono un albero bilanciato con nodi etichettati da 
elementi di lst, in modo tale che: 
preorder (balpreorder lst) = lst 
inorder (balinorder lst) = lst*)

let rec take n = function 
    [] -> [] 
  |x::rest -> if n=0 then [] 
    else x::take (n-1) rest 


let drop n lst =
  let rec aux tmp = function
     0 -> tmp 
    |n -> match tmp with 
         [] -> []
         |x::rest -> aux rest (n-1)  
  in aux lst n 


let rec balpreorder = function 
   []-> Empty
 | x::rest -> let a = (List.length rest)/2 in
       Tr(x,balpreorder (take a rest),balpreorder (drop a rest))

let rec balinorder = function 
    [] -> Empty
  |lst  -> let a = (List.length lst)/2 in 
           let (b::brest) = drop a lst in
           Tr(b, balinorder (take a lst), balinorder brest) 

(******************)
(*foglie_in_lista: 'a list -> 'a tree -> bool, data una lista lst e un 
albero t determini se ogni foglia di t appartiene a lst*)

let rec foglie_in_lista lst = function 
    Empty -> true 
  | Tr(x,Empty,Empty) -> List.mem x lst 
  |Tr(x,t1,t2) -> foglie_in_lista lst t1 && foglie_in_lista lst t2 


(****************)
(*num_foglie: 'a tree -> int che, applicata a un albero binario, riporti il
numero di foglie dell'albero*)

let rec num_foglie = function
     Empty -> 0
  |Tr(x,Empty,Empty) -> 1
  |Tr(_,t1,t2) -> num_foglie t1 + num_foglie t2 

(*******************)
(*segui_bool: bool list -> 'a tree -> 'a che, data una lista L di booleani e un 
albero binario T, riporti la radice del sottoalbero di T determinato da L, se
questo non è vuoto, altrimenti errore.*)

let rec segui_bool lst tree =
   match (lst,tree) with 
      ([],Empty) | (_,Empty) -> failwith "Errore" 
     |([], Tr(x,t1,t2)) -> x
     |(y::rest,Tr(x,t1,t2)) -> if y=true 
       then segui_bool rest  t1 
       else segui_bool rest  t2 


(*************************)
(*foglia_costo: int tree -> (int * int) che, dato un albero binario
 di interi, restituisca l'etichetta e il costo di una delle foglie 
più costose dell'albero *)

let rec foglia_costo  = function 
      Empty -> (0,0) 
  |Tr(x,Empty,Empty) -> (x,x)
  |Tr(x,t1,t2) ->  let (a,b) = foglia_costo t1 in 
                   let (c,d) = foglia_costo t2 in 
     if b>d then (a,x+b) 
     else (c,x+d) 

(******************************)
(*foglie_costi: int tree -> (int * int) list che, applicata a un albero
binario T etichettato da innteri, riporti una lista di coppie,
ciascuna delle quali ha forma (f,n), dove f è l'etichetta di una 
foglia in T e n il costo di tale foglia.*)

let rec foglie_costi = function 
   Empty -> []
  |Tr(x,Empty,Empty) -> [(x,x)]
  |Tr(x,t1,t2) -> let lst1 = foglie_costi t1 in 
                  let lst2 = foglie_costi t2 in
     List.map(function (a,b) -> (a,b+x))(lst1 @ lst2)

(*******************************************)
let trA = Tr("A"
      ,Tr("B"
        ,Tr("C",Empty,Empty)
        ,Tr("D",Empty,Empty)
        )
      ,Tr("E"
        ,Empty
        ,Tr("F",Empty,Empty)
        )
      );;



let trB = Tr("A"
      ,Tr("B"
        ,Tr("G",Empty,Empty)
        ,Empty
        )
      ,Tr("H"
        ,Empty
        ,Tr("F",Empty,Empty)
        )
      );;


let trB1 = Tr("A"
      ,Tr("B"
        ,Tr("G",Empty,Empty)
        ,Tr("K",Empty,Empty)
        )
      ,Tr("H"
        ,Empty
        ,Tr("F",Empty,Empty)
        )
      );;

(********************************)
(*max_common_subtree: string tree -> string tree -> string tree, che, dati
due alberi binari A e B, *i cui nodi sono etichettati da stringhe,
costruisca il massimo sottoalbero comune a A e B, partendo dalla radice:
i nodi di tale sottoalbero avranno la stessa etichetta che hanno i nodi 
corrispondenti in A e B, se essi sono uguali; altrimenti, se il nodo x
di A è diverso dal corrispondente nodo di B (o se uno dei due non c'è),
il nodo corrispondente a x nel massimo sottoalbero comune di A e B sarà
una foglia eitchettata "@"*)

let rec max_common_subtree tA tB =
 match (tA,tB) with 
    (Empty,Empty) -> Empty
  |(_,Empty) | (Empty,_) -> Tr("@",Empty,Empty)
  |(Tr(x,ta1,ta2),Tr(y,tb1,tb2)) -> if x=y 
    then Tr(x,max_common_subtree ta1 tb1,max_common_subtree ta2 tb2)
    else Tr("@",Empty,Empty)

(******************************)
(*dal compito d'esame settembre 2011*)
(*stessa_struttura: 'a tree -> 'a tree -> bool che determini se due alberi
hanno la stessa struttura*)

let rec s_s t1 t2 = 
match (t1,t2) with
     (Empty,Empty) -> true
  |(_,Empty) | (Empty,_) -> false
  |(Tr(x,s1,d1),Tr(y,s2,d2)) -> (s_s s1 s2) && (s_s d1 d2) 


(*esiste_mapping: 'a tree -> 'a tree -> bool che, applicata a due 
alberi binati t1 e t2 determini se esiste un mapping da t1 a t2. La funzione
non deve mai sollevare eccezioni ma deve sempre tornare un booleano.*)

let trA10 = Tr(1
      ,Tr(2
        ,Empty
        ,Empty
        )
      ,Tr(2
        ,Empty
        ,Empty   
        )
      );;


let trB10 = Tr(10
      ,Tr(20
        ,Empty
        ,Empty
        )
      ,Tr(20
        ,Empty
        ,Empty   
        )
      );;

let trC10 = Tr(10
      ,Tr(20
        ,Empty
        ,Empty
        )
      ,Tr(30
        ,Empty
        ,Empty   
        )
      );;

let rec lista_coppie ta tb = 
  match (ta,tb) with 
     (Empty,Empty) -> [] 
    |(Tr(x,ta1,ta2),Tr(y,tb1,tb2)) -> (x,y)::(lista_coppie ta1 tb1) 
                     @ (lista_coppie ta2 tb2) 
    |_ -> failwith "Strutture diverse" 

let rec is_function = function
    [] -> true
  | (x,y)::rest -> 
          try let z = List.assoc x rest 
              in z=y && is_function rest
          with Not_found -> is_function rest

let esiste_mapping t1 t2 = 
  try is_function (lista_coppie t1 t2)
  with _ -> false  


(*******)
(*dal compito di giugno 2011*)
(*path: ('a -> bool) -> 'a tree -> 'a list che, applicata a un predicato
p: 'a -> bool  e a un albero t: 'a tree, riporti, se esiste, un cammino dalla
radice alla foglia di t che non contenga alcun nodo che soddisfa p.
La funzione solleverà un'eccezione se un tale cammino non esiste.*)

(*check p:('a -> bool) -> 'a -> bool che ritora true se l'elemento x rispetta
il predicato, altrimenti false.*)


let rec path p = function 
     Empty -> failwith "path"
  |Tr(x,Empty,Empty) -> if p x then failwith "path"
    else [x]
  |Tr(x,t1,t2) ->  if  p x then failwith "path"
                   else x::(try path p t1
                            with Failure "path" -> path p t2)  
   


let p = (function x -> x = "B") 

(*****************************************************)
 

let subst = [(1,trA10);(2,trB10);(3,trC10)]

      
let rec applica subst = function 
     Empty -> Empty
  |Tr(x,Empty,Empty) -> 
          if List.exists (function (a,b) -> a=x) subst
          then  List.assoc x subst 
          else Tr(x,Empty,Empty) 
  |Tr(x,t1,t2) -> Tr(x, applica subst t1, applica subst t2) 


let trs = Tr(4
             ,Tr(1,Empty,Empty)
             ,Tr(6
                 ,Tr(2,Empty,Empty)
                 ,Tr(3,Empty,Empty)
                )
            )



(***********************************)
(*dal compito d'esame di febbraio 2010*)
(*path_coprente: 'a tree -> 'a list -> 'a list che, dato un albero A e una
lista di elementi dello stesso tipo dei nodi di A, restituisca, se esite,
un ramo dell'albero dalla radice a una foglia che contenga tutti i nodi
di L ed eventualmente anche altri nodi. Se un tale cammino non esiste,
il programma solleverà un'eccezione. Si assuma che L sia senza ripetizioni*)

let rec togli x = function 
    [] -> []
  |y::rest -> if x=y then rest
    else y::togli x rest 


let rec path_contenente tr lst  =
   match tr with 
       Empty -> failwith "path"
     |Tr(x,Empty,Empty) -> if lst=[] || lst=[x] then [x] 
                           else failwith "path"
     |Tr(x,t1,t2) -> let newlist = togli x lst in
                     x::(try path_contenente t1 newlist
                         with _ -> path_contenente t2 newlist)


let tr6 = Tr(0
      ,Tr(10
        ,Tr(2,Empty,Empty)
        ,Tr(5,Empty,Empty)
        )
      ,Tr(6
        ,Tr(6
          ,Tr(3,Empty,Empty)
          ,Empty
          )
        ,Tr(4
          ,Tr(3,Empty,Empty)
          ,Tr(4,Empty,Empty)
          )
        )
      );;

(**************************)
(*esame luglio 2009*)

let tr14 = Tr(1
      ,Tr(2
        ,Tr(8,Empty,Empty)
        ,Tr(3,Empty,Empty)
        )
      ,Tr(5
        ,Tr(8
          ,Tr(10,Empty,Empty)
          ,Tr(9,Empty,Empty)
          )
        ,Tr(7
          ,Tr(4,Empty,Empty)
          ,Tr(8,Empty,Empty)
          )
        )
      )

 type col = Rosso | Giallo | Verde | Blu
 type 'a col_assoc = (col * 'a list) list

(*colore: 'a -> 'a col_assoc -> col che, dato un valore x di tipo 'a e una
lista che rappresenta un'associazione di colori, riporti il colore di x,
se tale colore è definito, sollevi un'eccezione altrimenti.*)

let colori = [(Some Rosso,[1;2;3;4;7;10]);(Some Giallo,[3;8;11]); 
     (Some Verde,[0;5;6;13]);(Some Blu,[9;12;14;15])]

let rec colore y = function
   [] -> failwith "colore"
  |(a,b)::rest ->
                   if List.mem y b then a
                   else colore y rest 

(*Dichiarare un tipo di dati per rappresentare alberi binare e 
scrivere una funzione pat_to: 'a -> 'a col_assoc -> 'a tree -> 'a list
che, dato un valore x: 'a, un'associazione di colori e un albero binario,
riporti- se esiste- un ramo a colori alterni, dalla radice dell'albero
a una foglia etichettata da x. Se un tale ramo non esiste solleverà
un'eccezione.*)
(*togli2: 'a -> 'a col_assoc -> 'a list, ritorna una lista senza la coppia 
che contiene x nel secondo elemento della coppia, altrimenti torna la lista.*)
let path_to y lst tr =
  let rec aux colore_padre = function 
         Empty -> failwith "aux" 
    |Tr(x,Empty,Empty) -> let colore_figlio = colore x lst in 
     if (x=y) && Some (colore_padre) <> Some (colore_figlio)
     then [x]
     else failwith "aux"
    |Tr(x,t1,t2) -> let colore_figlio = colore x lst in 
     if Some (colore_padre) = Some (colore_figlio)
     then failwith "aux"
     else  x::(try aux colore_figlio t1
              with _ -> aux colore_figlio t2)
     in aux None tr
(*******************************************)
(*abr_check: ('a * 'b) tree -> bool che controlli se un albero è un ABR.*)


let a_wiki = Tr((8,"A")
      ,Tr((3,"B")
        ,Tr((1,"C"),Empty,Empty)
        ,Tr((6,"D")
          ,Tr((4,"E"),Empty,Empty)
          ,Tr((7,"F"),Empty,Empty)
          )
        )
      ,Tr((10,"G")
        ,Empty
        ,Tr((14,"H")
          ,Tr((13,"I"),Empty,Empty)
          ,Empty
          )
        )
      )

(*(<<): 'a -> 'a option -> bool*)
(*(>>): 'a -> 'a option -> bool*)


let (<<) a = function
    None -> true
  | Some b -> a<b

let (>>) a = function 
    None -> true 
  | Some b -> a>b 

let abr_check t = 
  let rec aux minv maxv = function 
      Empty -> true 
    | Tr((x,_),t1,t2) -> 
      x>> minv && x<< maxv &&
      aux minv (Some x) t1 &&
      aux (Some x) maxv t2
  in aux None None t  


(*15b*)

(*abr_search: ('a * 'b)tree -> 'a -> 'b che, dato un abr e una chiave k riporti
il valore v associato a k nell'albero, un errore altrimenti.*)

let rec abr_search tr k = 
  match tr with  
    Empty -> failwith "abr"
  |Tr((a,b),t1,t2) -> if a = k then b 
                      else 
                      try abr_search t1 k
                      with _ -> abr_search t2 k 

(*abr_update: ('a * 'b) tree -> ('a * 'b) -> 'a tree che, dato un abr T e una
coppia (k,v), riporti l'abr che si ottiene da t aggiungendo (k,v). Se t gia
contiene un elemento con chiave k, il suo valore verrà sostituito con v*)

let rec abr_update tr (k,v) = 
match tr with 
    Empty -> Tr((k,v),Empty,Empty)
  |Tr((a,b),t1,t2) -> if a=k then Tr((k,v),t1,t2)
    else
    if k < a then Tr((a,b), abr_update t1 (k,v),t2) 
    else Tr((a,b),t1, abr_update t2 (k,v)) 


let a_wiki2 = Tr((8,8)
      ,Tr((3,3)
        ,Tr((1,1),Empty,Empty)
        ,Tr((6,6)
          ,Tr((4,4),Empty,Empty)
          ,Tr((7,7),Empty,Empty)
          )
        )
      ,Tr((10,10)
        ,Empty
        ,Tr((14,14)
          ,Tr((13,13),Empty,Empty)
          ,Empty
          )
        )
      )

(*****15d*******)
(*abr_delmin: 'a tree -> 'a * 'a tree che, dato un ABR T riporti la coppia
label,T') dove label (una coppia chiave-valore) è l'etichetta di T con
chiave minima e T' è l'albero che si ottiene da T eliminando il nodo 
etichettato da label. Ovviamente, se T è vuoto, si sollleverà 
un'eccezione.*)

let rec abr_delmin = function 
     Empty -> failwith "delmin"  
  |Tr(x,Empty,t2) -> (x,t2)
  |Tr(x,t1,t2) -> let (a,sx) = abr_delmin t1 
                  in (a,Tr(x,sx,t2))
   


(***15e***) 
(*abr_delete: ('a * 'b)tree -> 'a -> ('a * 'b)tree che implementi
l'algoritmo di cancellazione di un elemento da un abr: dato un abr T
e una chiava k, costruire l'abr che si ottiene da T cancellando 
l'elemento con chiave k(se esiste, e lasciando T immutato altrimenti).*)


let rec abr_delete tr y = 
match tr with 
    Empty -> Empty 
  |Tr((k,v) as x ,t1,Empty) ->
                           if k=y then t1 
                           else Tr(x,abr_delete t1 y,Empty) 
  |Tr((k,v) as x ,Empty,t2) -> if k=y then t2 
                          else Tr(x,Empty,abr_delete t2 y)
  |Tr((k,v) as x ,t1,t2) ->
    if k=y then let (nodo,albero)= abr_delmin t2 
    in Tr(nodo,t1,albero)
    else Tr(x, abr_delete t1 y, abr_delete t2 y)
