(*====ESERCIZIO 1====*)

let ultime_cifre n = 
  let y = abs n
  in ((y/10) mod 10 , y mod 10)

(*====ESERCIZIO 2====*)

let  bello n =
 if(n> -10 && n<10) then
 match abs n with
  0|3|7->true
  |_ -> false 
 else
   let (x,y) = ultime_cifre n
   in bello y && not( bello x)

(*====ESERCIO 3====*)

(*  let data (d,m) =
    if(d>0 && d<32) then 
    match m with 
 *  "gennaio"|"marzo"|"maggio"|"luglio"|"agosto"|"ottobre"|"dicembre" ->true
    |_ ->false
    else 
 * if(d>0 && d<31) then 
    match m with 
    "aprile"|"giugno"|"settembre"|"novembre"->true
 *  |_->false
 *  else 
 *  if(d>0 && d<30) then
 *  match m with
 * "febbraio"->true
    |_ ->false*)
let data (d,m)=
 d>0 && 
 match m with 
 "gennaio"|"marzo"|"maggio"|"luglio"|"agosto"|"ottobre"|"dicembre" -> d<=31
 |"aprile"|"giugno"|"settembre"|"novembre"->d<=30
 |"febbraio"->d<=28
 |_->false 




