type 'a ntree = Ntree of 'a * 'a ntree list 
let leaf x = Ntree (x,[])

(****2****)
(*postorder: 'a ntree -> 'a list e inorder: 'a ntree -> 'a list che,
dato un albero n-ario, riportino la lista dei suoi nodi nell'ordine
in cui sarebbero visitati secondo i due algortimi di visita.*)

let t2 = Ntree(1,[Ntree(2,[Ntree(3,[leaf 4;
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


let rec postordine = function 
   Ntree(x,tlist) ->
        List.flatten(List.map postordine tlist) @ [x]


let rec inorder = function 
    Ntree(x,[]) -> [x] 
  | Ntree(x,t::rest) -> 
        inorder t@
        [x]@
        (List.flatten(List.map inorder rest))

(***es3***)
(*foglie_in_lista: 'a list -> 'a ntree -> bool che, data una lista lst
e un albero n-ario t, determini se ogni foglia di t appartiene a lst*)

let rec foglie_in_list lst = function 
      Ntree(x,[]) -> List.mem x lst
  |Ntree(x,tlist) ->
         List.for_all (foglie_in_list lst)tlist 

(***es4***)
(*sum_leaf: int list -> int che, data la lista di foglie, riporta
la somma di queste ossia il numero totale di foglie dell'albero.*)

let rec sum_leaf = function 
   [] -> 0
  |x::rest -> x + sum_leaf rest 

(*num_foglie: 'a ntree -> int che, applicata a un albero n-ario, riporti il 
numero di foglie dell'albero*)

let rec num_foglie = function 
      Ntree(x,[]) -> 1
  |Ntree(x,tlist) -> sum_leaf (List.map (num_foglie) tlist)
          

(***es5***)
(*listaGuida: 'a list -> 'a ntree -> 'a che, data una lista L
di interi e un albero n-ario T, riporti la radice del sottoalbero 
di T determinato da L, se L determina un sottoalbero T,ù
un errore altrimenti.*)

let rec listaGuida lst t = 
  match (lst,t) with
      ([],Ntree(x,_)) -> x
    |(x::rest,Ntree(_,tlist)) ->
            listaGuida rest (List.nth tlist x)     

(*es6*)
(*max_leaf: (int * int) list -> (int * int) che, applicata a una lista
di coppie mi ritorna la coppia con secondo elemento massimo della lista.*)

let max_leaf (z::rest) = 
  let rec aux massimo = function 
      [] -> massimo
    |(a,b)::rest ->
      if b > (snd z) then aux (a,b) rest 
      else aux massimo rest  
  in aux z rest

(*foglia_costo: 'int tree -> (int * int) che, dato un albero n-ario di
interi, restituisca l'etichetta e il costo della foglia più costosa
dell'albero. L'albero può anche avere nodi diversi con la stessa etichetta.*)

let rec foglia_costo = function 
    Ntree(x,[]) -> (x,x)
  |Ntree(x,tlist) -> 
     let (a,b) =  max_leaf (List.map foglia_costo tlist)
     in (a,b+x) 



(***es7***)


let rec tutte_foglie_costi = function
     Ntree(x,[]) -> [(x,x)]
  |Ntree(x,tlist) ->
      List.map(function (a,b) -> (a,b+x))
     (List.flatten(List.map tutte_foglie_costi tlist))


(***es8***)
(*ramo_della_lista: 'a ntree -> 'a list -> 'a -> 'a list che, dato
un albero T, una lista senza ripetizioni e un'etichetta k, riporti,
se esiste, un ramo di T dalla radice a una foglia etichettata da k
che passo per tutti gli elementi di L esattamento una volta e contenga
solo nodi etichettati da elementi di L.Se non esiste eccezione*)


let ramo_della_foglia tr lst k = 
  let rec aux tmp = function
     Ntree(x,[]) -> if x = k then [x]
                   else failwith "ramo"
  |Ntree(x,tlist) ->
   if  List.mem x lst 
   then (List.flatten (aux x::tmp tlist))
  in aux [] tr















