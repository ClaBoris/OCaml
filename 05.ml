(*1a*)
let in_labirinto dim (r,c) = 
 r>=0 && c>=0 && r<dim && c<dim

let filter_vicini dim list = 
  let rec aux tmp = function 
     [] -> tmp
    |c::rest ->
      if in_labirinto dim c then aux (c::tmp) rest 
      else aux tmp  rest
  in aux [] list

(*1b*)
(*cobine: 'a list -> 'b list -> ('a * 'b)list*)
(*ritorna una lista di coppie in cui il primo elemento appartiene 
alla prima lista ed il secondo alla seconda*)

(* let rec combine lst1 lst2 = 
  match (prima , seconda) with 
      ([],[]) -> []
 *  |(x::rest1 , y::rest2) -> (x,y)::combine rest1 rest2
    |_ -> failwith "combine"*)


(*differenza*)
let rec differenze set = function 
     []->[]
  |x::rest ->
    if List.mem x set then differenze set rest
    else x::differenze set rest


(*************)


let rec setminus set1 set2 =
  match set1 with
    [] -> []
  | x::rest -> 
      if List.mem x set2 then setminus rest set2
      else x::setminus rest set2
(***************)
let rec trips = function 
   x::y::z::rest -> (x,y,z)::trips(y::z::rest)
  |_-> []

(**********)
let rec take n = function 
  []->[]
  |x::rest ->
      if n=0 then []
      else x::take(n-1)rest

let rec choose k lst = 
if k > List.length lst  then []
else (take k lst) :: choose k (List.tl lst) 
(************)

let strike_ball test guess = 
  let rec aux strike ball t g = 
  match (t,g) with 
    ([],[]) -> (strike,ball)
    |(x::trest,y::grest)->
      if x=y then aux  strike (ball+1) trest grest
      else 
      if List.mem x guess then aux(strike+1) ball trest grest
      else aux strike ball trest grest 
  in aux 0 0 test guess 

