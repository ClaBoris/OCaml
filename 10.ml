
type 'a graph1 = ('a * 'a)list

let grafo1a = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6); (5,4); (6,5); (6,7)]

let grafo1b = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6); (5,4); (6,5)]

(*successori: 'a -> 'a graph -> 'a list che dato un elemento x
e un grafo g, torna la lista dei successori di x se questo appartiene
al grafo orientato g.*)
let rec successori x = function 
   [] -> [] 
  |(a,b)::rest -> 
       if x=a
       then b::successori x rest
       else successori x rest

(*test_connessi: 'a graph -> 'a -> 'a ->bool che,dato un grafo orientato
G e due nodi N e M, determini se esiste un cammino da N a M. La funzione
riporta un booleano.*)
let test_connessi g n m = 
  let rec aux visited = function
     [] -> false
    |x::rest -> 
         if List.mem x visited
         then aux visited rest
         else x = m || aux (x::visited) (successori x g @ rest)
  in aux [] [n] 


(*esiste_ciclo: 'a graph -> 'a -> bool che, dato un grafo orientato G e 
un nodo N, determini se esiste un ciclo su N (cioè un cammino da N a N
che contenga almeno un arco).*)

let esiste_ciclo g n = 
  let rec aux visited = function
     [] -> false
    |x::rest -> 
        if List.mem x visited
        then x=n
        else aux (x::visited) (successori x g @ rest) 
  in aux [][n]



(************************************)
(*es3*)
(*ciclo: 'a graph -> 'a -> 'a list che, dato un grafo orientato
G e un nodo N, riporti, se esiste, un ciclo su N, altrimenti sollevi
un'eccezione.*)

let g1_ciclo = [(1,3);(3,5);(5,4);(4,1);(2,5);(2,6);(6,5);(6,7)]

let ciclo g start = 
  let rec aux visited n = 
   if List.mem n visited 
   then if n = start 
        then List.rev visited
        else failwith "aux"
   else auxlist (n::visited) (successori n g) 
  and auxlist visited = function 
     [] -> failwith "auxlist"
    |x::rest -> 
      try aux visited x
      with _ -> auxlist visited rest
  in aux [] start

(****************************************)
(*es4*)

type 'a graph2 = 'a list * ('a * 'a) list 

let grafo2 = ([1;2;3;4;5],[(1,2);(1,4);(1,5);
              (2,4);(2,3);(3,4);(4,5)])

(*vicini: 'a -> 'a graph -> 'a list*)
(*vicini nodo g = lista dei vicini di n nel grafo non orientato g*)
 
let rec vicini nodo = function 
    [] -> []
  |(x,y)::rest -> 
     if nodo = x 
     then y::vicini nodo rest
     else if nodo = y 
          then x::vicini nodo rest
          else vicini nodo rest  
(*esiste_cammino: 'a graph -> 'a -> 'a -> bool*)
(*esiste_cammino g n m = true se esiste un cammino tra n e m
nel grafo non orientato g*)
(*aux: 'a list -> 'a list -> bool *)
(*aux visited listanodi = true se da un nodo di listanodi
si può raggiungere m senza passare per i nodi di visited*)


let esiste_cammino g n m = 
  let rec aux visited = function
     [] -> false
    |x::rest -> 
      if List.mem x visited 
      then aux visited rest
      else x=m || aux (x::visited)((vicini x (snd g))@rest) 
  in aux [] [n]

(*grafo_connesso: 'a graph2 -> bool che, dato un grafo non orientato G,
determini se G è connesso*)

let grafo_connesso (nodi,archi) = 
  let n = List.hd nodi in 
  List.for_all(esiste_cammino archi n)(List.tl nodi)


(*****************************************)
(*giugno2021*)

let g1_2021=[(1,2);(1,4);(2,4);(4,5);(4,3);(4,4);
           (3,2);(3,6);(5,6);(6,6)]

let depth_limited g start goal depth = 
  let rec aux visited n d=
     if d <= depth 
     then if n = goal 
          then List.rev (n::visited)
          else auxlist (n::visited)(d+1)(successori n g)
     else failwith "aux"
  and auxlist visited new_depth = function 
     [] -> failwith "auxlist"
    |x::rest -> try aux visited x new_depth
                with _ -> auxlist visited new_depth rest
  in aux [] start 0


(******************************************)
(*febbraio 2009*)
(*cammino: 'a graph -> 'a list -> 'a -> 'a -> 'a list che,dato un grafo G,
 una lista L senza ripetizioni e due nodi n e m di G, riporti, se esiste,
un cammino da n a m che passo solo per nodi contenuti in L e per ciascuno di
essi una volta. Se tale cammino non esiste, la funzione solleverà
 un'eccezione.*)

(*delete: 'a -> 'a list -> 'a list tale che, delete n lst = funzione
che elimina l'elemento n dalla lista lst e ritorna la lista senza n.*)

let rec delete n = function 
   [] -> []
  |x::rest -> 
       if n = x 
       then delete n rest 
       else x::delete n rest 


let cammino g lst n m =
  let rec aux visited n lst = 
    if not(List.mem n lst) 
    then failwith "aux"
    else let new_lst = delete n lst in 
         if n = m 
         then if lst = [] 
              then List.rev (n::visited)
              else failwith "aux"
         else auxlist (n::visited) new_lst (successori n (snd g)) 
  and auxlist visited new_list = function 
      [] -> failwith "auxlist" 
    |x::rest -> try aux visited x new_list
                with _-> auxlist visited new_list rest
  in aux [] n lst 


(***************************************)
(*settembre 2020*)
(*percorso: 'a graph -> 'a -> 'a -> 'a -> 'a -> 'a list tale che,
percorso g start tappa target riporti, se esite, un cammino aciclico 
nel grafo orientato g dal nodo start fino al nodo target, che 
passi per il nodo tappa. La funzione solleverà un'eccezione se
un tale cammino non esiste, ma deve riportare un cammino anche 
nel caso in cui start=tappa e/o tappa=target.*)

let percorso g start tappa goal = 
  let rec aux visited flag n = 
   if List.mem n visited
   then failwith "aux"
   else if n = tappa
        then if n = goal 
             then List.rev(n::visited)
             else  auxlist(n::visited) 1 (successori n g) 
        else if n = goal 
             then if flag = 0 
                  then failwith "aux1"
                  else List.rev(n::visited)
             else auxlist (n::visited) flag (successori n g) 
 and auxlist visited flag = function 
      [] -> failwith "auxlist"
    |x::rest -> try aux visited flag x 
                with _ -> auxlist visited flag rest 
  in aux [] 0 start






