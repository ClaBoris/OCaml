type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree 
type 'a ntree = Ntree of 'a * 'a ntree list 
let leaf x = Ntree(x,[]) 


let tr1b = Tr("A",
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


let trA10 = Tr(1
      ,Tr(2
        ,Empty
        ,Empty
        )
      ,Tr(2
        ,Empty
        ,Empty   
        )
      )

let trB10 = Tr(10
      ,Tr(20
        ,Empty
        ,Empty
        )
      ,Tr(20
        ,Empty
        ,Empty   
        )
      )


let trC10 = Tr(10
      ,Tr(20
        ,Empty
        ,Empty
        )
      ,Tr(30
        ,Empty
        ,Empty   
        )
      )




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
       )




let nt1 = Ntree(1,[Ntree(2,[Ntree(3,[leaf 4;
                          leaf 5]);
                    Ntree(6,[leaf 7]);
                    leaf 8]);
              leaf 9;
              Ntree(10,[Ntree(11,[leaf 12;
                            leaf 13;
                            leaf 14]);
                     leaf 15;
                     Ntree(16,[leaf 17;
                            Ntree(18,[leaf 19;
                                   leaf 20])])])])




let nt2 = Ntree(2,[Ntree(2,[Ntree(3,[leaf 4;
                          leaf 5]);
                    Ntree(6,[leaf 7]);
                    leaf 8]);
              leaf 9;
              Ntree(10,[Ntree(11,[leaf 12;
                            leaf 13;
                            leaf 14]);
                     leaf 15;
                     Ntree(16,[leaf 17;
                            Ntree(18,[leaf 19;
                                   leaf 20])])])])



let nt3 = Ntree(1,[Ntree(2,[Ntree(3,[leaf 4;
                          leaf 5]);
                    Ntree(6,[leaf 7]);
                    leaf 8]);
              leaf 9;
              Ntree(10,[Ntree(11,[leaf 12;
                            leaf 13;
                            leaf 14]);
                     leaf 15;
                     Ntree(16,[leaf 17;
                            Ntree(18,[leaf 19])])])])

(**********************************)
(*settembre2011*)
(*esA*)

(*stessa_struttura: 'a tree -> 'a tree -> bool che determini se 
due alberi binari hanno stessa struttura cioè se essi sono uguali
quando si ignorano le etichette*)

let rec stessa_struttura tr1 tr2 = 
match (tr1,tr2) with 
   (Empty,Empty) -> false
  |(Tr(_,Empty,Empty),Tr(_,Empty,Empty)) -> true
  |(Empty,_) | (_,Empty) -> false
  |(Tr(x,ta1,ta2),Tr(y,tb1,tb2)) -> 
        (stessa_struttura ta1 tb1) && (stessa_struttura ta2 tb2)


(*esB*)
(*esiste_mapping: 'a tree -> 'a tree -> bool che, applicata a due alberi
binari t1 e t2 determini, se esiste, un mapping da t1 a t2. La funzione non
deve mai sollevare eccezioni, ma deve riportare sempre un booleano.*)

let rec coppie tr1 tr2 = 
match (tr1,tr2) with 
   (Empty,Empty) -> []
  |(_,Empty) | (Empty,_) -> failwith "coppie"
  |(Tr(x,tx1,tx2),Tr(y,ty1,ty2)) -> 
      (x,y)::(coppie tx1 ty1 @ coppie tx2 ty2)
 
(*check: ('a * 'b) -> ('a * 'b) list -> bool che, data una coppia(a,b) e 
una lista di coppie, torna true se il valore a associa uno e un solo valore
b.*)
let rec check_coppie = function
  [] -> true
 |(a,b)::rest -> 
  not(List.exists (function (x,y) -> a=x && b<>y) rest) && check_coppie rest  


let rec esiste_mapping t1 t2 = 
try  check_coppie (coppie t1 t2)
with _ -> failwith "mapping"


(************************************************)
(*giugno2011*)
(*path: ('a -> bool) -> 'a tree -> 'a list che, applicata a un predicato
p: 'a -> bool e a un albero t: 'a tree, riporti,se esiste, un cammino 
dalla radice a una foglia di t che non contenga alcun nodo che soddisfa p.
La funzione solleverà un'eccezione se un tale cammino non esiste.*)

let rec path p = function 
   Empty -> []  
  |Tr(x,t1,t2) -> 
     if not(p x)  
     then x::try path p t1 
             with _ -> path p t2 
     else failwith "path"  


let p = (function x -> x=2)

(****************************************)
(*es7gruppo8*)
(*tutte_foglie_costi: int ntree -> (int * int) list che, applicata a un
albero n-ario T etichettato da interi, riporti una lista di coppie,
ciascun delle quali ha la forma (f,n), dove f  è l'etichetta di una
foglia in T e n il costo di tale foglia.*)



let rec tutte_foglie_costi = function 
   Ntree(x,[]) -> [(x,x)]
  |Ntree(x,tlist) ->  
     List.map (function (a,b) -> (a,b+x))
              (List.flatten (List.map tutte_foglie_costi tlist)) 


(****************************************************)

(*same_structure: 'a ntree -> 'b ntree -> bool che determini se due alberi 
n-ari hanno la stessa struttura*)

let rec same_structure nt1 nt2 = 
  match (nt1,nt2) with 
     (Ntree(_,[]),Ntree(_,[])) -> true
    |(Ntree(x,tlistx),Ntree(y,tlisty)) -> 
         auxlist tlistx tlisty
and auxlist lst1 lst2 = 
match (lst1,lst2) with 
    ([],[]) -> true
  |(ta::resta,tb::restb) -> same_structure ta tb && auxlist resta restb
  |_ -> false   


(************************************)
type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list

let root (Ntree(x,_)) = x

let ramo_colorato n lst ntr = 
  let rec aux colore_padre = function
       Ntree(x,[]) -> if x = n then [x]
                       else failwith "aux"
    |Ntree(x,tlist) -> x::auxlist (colori x lst) tlist 
  and auxlist colore_prec = function
       [] -> failwith "auxlist"
  |t::trest ->
     try 
         if colore_prec <> (colori (root t) lst) 
         then  aux (colori (root t) lst) t
         else failwith "auxlist"
    with _ ->  auxlist colore_prec trest
  in aux (colori (root ntr) lst) ntr 


let lista_colori = [(Rosso,[1;15;9;11;17]);(Giallo,[2;6;10;19]);
(Verde,[3;7;8;12;14;16;20]);(Blu,[4;5;13;18])]


(***********************************)
let rec foglie_costi = function 
      Empty -> [] 
  |Tr(x,Empty,Empty) -> [(x,x)]
  |Tr(x,t1,t2) -> 
  List.map (function (a,b)->(a,b+x)) 
           (foglie_costi t1 @ foglie_costi t2)

(************************************)

let tp = Tr (3, Tr (2, Tr (1, Empty, Empty),
   Empty),
   Tr (5, Tr (4, Empty, Empty),
  Empty))



let path_coprente t lst = 
  let rec aux  = function 
      Empty -> failwith "aux"
    |Tr(x,Empty,Empty) -> if List.mem x lst 
      then [x]
      else failwith "aux"
    |Tr(x,t1,t2) ->
              try  x::path_list t1
              with _ -> x::path_list t2
  and path_list = function 
      Empty -> failwith "path"
    |Tr(x,Empty,Empty) -> if List.mem x lst then [x]
      else failwith "path"
    |Tr(x,t1,t2) -> 
           try aux t1
           with _ -> path_list  t2
 in aux t
