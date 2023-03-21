(***1a***)

let rec find p = function 
    []-> failwith "find"
  |x::rest ->
    if p x then x
    else find  p rest 

let find_applicata lst = 
find (fun x -> x * x < 30) lst

(***1b***)

let rec takewhile p = function 
    [] -> [] 
  |x::rest -> 
     if p x then x::takewhile p rest 
     else []

(***1c***)
let rec dropwhile p = function 
    [] -> []
  |x::rest -> 
    if p x then dropwhile p rest
    else x::rest 

(***1d***)
let partition p lst =
  let rec aux y n = function 
     [] -> (y,n)
    |x::rest ->
       if p x then aux (x::y) n rest 
       else aux y (x::n) rest   
  in aux [] [] lst



(***1e***)

let pairwith y lst = 
List.map(function x -> (y,x)) lst 

(***1f***)
let verifica_matr n mat =
List.exists (List.for_all (function x -> n>x)) mat 


(***1g***)
let setdiff set1 set2 =
List.filter (function x ->  not (List.mem x set2) ) set1

(***1h***)
let subset set1 set2 = 
List.for_all (function x -> (List.mem x set2) )set1


(***1i***)
let suplica lst1 =
List.map (function x-> 2*x)lst1


(***1j***)

(*('a * 'b list)list -> 'b -> ('a * 'b list) list, che data una lista
di coppie L e un elemento x: 'b riporta  la lista che si ottiene inserendo
x in testa a ogni secondo elemento delle coppie L.*)

let mapcons lst1 k = 
List.map (function (x,y) -> (x, k::y) ) lst1


(***1k***)
(*tutte_liste_con : int -> 'a -> 'a -> 'a list list, che dato un intero non 
negativo n e due valori (dello stsso tipo) x e y, riporta una lista contenente
tutte le possibile liste di lunghezza n contenenti soltanto i due valori 
x e y*)

let cons x rest = x::rest


let rec tutte n x y =
  if n=0 then [[]] 
  else let tmp = tutte (n-1) x y 
  in (List.map (cons x) tmp) @ (List.map (cons y) tmp)

(***1l***)

(*'a -> 'a list -> 'a list list , interleave x lst riporta una lista 
con tutte le liste che si ottengono inserendo x in qualsiasi posizione 
in lst.*)

let cons x rest = x::rest

let rec  interleave x  = function  
 []  -> [[x]]
|y::rest -> 
    (x::y::rest) :: (List.map (cons y) (interleave x rest)) 

(***3a***)
(*find: 'a -> 'a list -> 'a list * 'a list che applicata a un elemento 
x e una lista L, spezzi L in due parti: la prima contiene tutti gli elementi
che vanno dall'inizio della lista fino alla prima occorrenza di x esclusa; la 
seconda contiene tutti gli elementi che seguono la prima occorrenza di x.
Se non contiene x la funzione solleva un'eccezione*)

(*****) 

let compito lst1 lst2 = 
List.for_all (function x -> (List.exists (function y -> x mod y = 0 )lst2))lst1


(*****)
let rec compito1  lst1 lst2 = 
 match (lst1,lst2) with
    ([],[]) -> []
   |(x::rest1,y::rest2) -> if (x mod y = 0) then x::compito1 rest1 lst2
     else compito1 rest1 lst2   
