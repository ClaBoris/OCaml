

let lista1=[1;2;3]
let lista2=[4;5;6]
(*******ESERCIZIO 1a*******)
(*length: 'a list -> int*)
(*riporta il numero di elementi in una lista*)

let rec length = function 
   [] -> 0 
  | _:: rest -> 1 + length(rest)

(*****ESERCIZIO 1b******)

(*sumof: int list -> int*)
(*riporta la somma degli interi in una lista*)

let rec sumof = function
   [] -> 0
  |x::rest -> x + sumof (rest)


(****ESERCIZIO 1c*****)

(*maxlist: 'a list -> 'a*)
(*aux calcola il massimo elemento della lista *)

let rec maxlist = function
   []->failwith "Lista vuota"
  | [x] -> x
  | x::y::rest -> maxlist ((max x y)::rest)

(****ESERCIZIO 1d******)
(*drop: int->'a list -> 'a list*)


let  drop n  lst = 
  let rec aux acc = function
     0 -> acc 
    |n -> match acc with 
          []->[]
         |x::rest -> aux rest (n-1) 
  in aux lst n 
(***Esercizio 1e***)
(*append: int -> 'a list -> 'a list*)
(*append: concatena due liste: @*)

let append_it lst1 lst2 =
  let rec aux tmp = function 
     [] -> tmp 
    |x::rest -> aux (x :: tmp) rest  
  in aux lst2 (List.rev lst1) 

(***ES 1f***)
(*reverse: 'a list -> 'a list*)

let rec reverse = function 
   [] -> [] 
  |x::rest  -> (reverse rest)@ [x] 

(***ESERCIZIO 1g***)

let rec nth n = function 
  [] -> failwith "Errore"
  |x::rest -> 
     if n=0 then x 
     else nth (n-1) rest 


let nth1 n lst = 
  let rec aux tmp = function
   [] -> failwith "Errore"
  |x::rest -> if n = 0 then x 
    else aux (n-1) rest 
  in aux n lst

(***ESE 1h***)
let rec remove x = function 
    []->[]
  |y::rest -> if x=y then remove x rest 
    else y::remove x rest  

let remove1 x lst = 
  let rec aux tmp = function 
      [] -> List.rev tmp  
    |y::rest -> if x=y then aux tmp rest  
      else aux (y::tmp) rest
  in aux [] lst  



(***ESERCIZIO2a***)

(*copy : int -> 'a -> 'a list *)

(*copy riporta la lista di lunghezza n i cui elementi sono tutti uguali a x*)

let copy n x = 
  let rec aux tmp k = 
     if k <= 0 then tmp 
     else aux (x::tmp) (k-1)  
  in aux [] n


let rec copy1 n x = 
  if n<=0 then [] 
  else x::copy (n-1) x

(***2b***)

let rec nondec =  function 
   []|[_] -> true
  |x::y::rest -> x<=y && nondec rest 

(***2c***)
let rec pairwith y = function 
    [] -> [] 
  |x::rest -> (y,x) :: pairwith y rest 

(***2d***)

let rec duplica = function 
   []->[]
  |x::rest -> x::x::duplica rest 
(***2e***)

let enumera lst =
  let rec aux count = function 
     [] -> []
    |x::rest -> (count,x) :: aux (count+1) rest
  in aux 0 lst 


(***2f***)

let position x lst = 
  let rec aux count = function
    [] -> failwith "Errore"
  |y::rest ->
     if x = y then count 
    else aux (count+1) rest 
  in aux 0 lst  

(***2g***)

let rec alternate = function 
   [] | [_] -> []
  |_::y::rest -> y :: alternate rest 


(***2h***)

let rec min_dei_max = function 
   [] -> failwith "Errore"
  |[lst] -> maxlist lst
  |lst::rest -> min (maxlist lst) (min_dei_max rest) 

(***2i***)
