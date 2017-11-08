let rec pow_fl fl = function

  (* float -> int -> float *)

  (* Fonction puissance, appliquée à un flottant
     Utilise l'exponentiation rapide *)

  |0 -> 1.
  |1 -> fl
  |n -> pow_fl fl (n mod 2) *. r *. r where r = pow_fl fl (n/2)
;;

let rec pow x = function

  (* int -> int -> int *)

  (* Fonction puissance, appliquée à un entier
   Utilise l'exponentiation rapide *)

  |0 -> 1
  |1 -> x
  |n -> pow x (n mod 2) * r * r where r = pow x (n/2)
;;

let abs_fl = function

  (* float -> float *)

  (* Renvoie la valeur absolue d'un flottant *)

  |x when x >= 0. -> x
  |x              -> -.x
;;

let rec somme_liste = function

  (* float list -> float *)

  (* Fais la somme de tous les flottants de la liste (analogue de apply) *)

  |[]   -> 0.
  |t::q -> t +. somme_liste q
;;

let norme1 (x,y) =

  (* point -> float *)

  (* Renvoie la norme 1 d'un point du plan dont les coordonnées sont des flottants *)
  
  max (abs_fl x) (abs_fl y)
;;

let distance1 (x1,y1) (x2,y2) =

  (* point -> point -> float *)

  (* Renvoie la distance liée à la norme 1 entre deux points dont les coordonnées sont des flottants *)
  
  norme1 (x1-.x2,y1-.y2)
;;

let fst_4 (e,_,_,_) =

  (* 'a * 'b * 'c * 'd -> 'a *)

  (* Renvoie la première composante d'un quadruplet *)
  
  e
;;

let snd_4 (_,e,_,_) =
  
  (* 'a * 'b * 'c * 'd -> 'b *)

  (* Renvoie la deuxième composante d'un quadruplet *)
  
  e
;;

let trd_4 (_,_,e,_) =

  (* 'a * 'b * 'c * 'd -> 'c *)

  (* Renvoie la troisième composante d'un quadruplet *)
  
  e
;;

let fth_4 (_,_,_,e) =
  
  (* 'a * 'b * 'c * 'd -> 'd *)

  (* Renvoie la quatrième composante d'un quadruplet *)
  
  e
;;

let rec int_of_float' x =

  (* float -> int *)

  (* Renvoie l'entier le plus proche (translation de la fonction partie entière) *)

  if x >= 0.
  then int_of_float (x+.0.5)
  else - int_of_float' (-.x)
;;

let float_of_int_2 (a,b) =

  (* point -> point *)

  (* Applique float_of_int à un point du plan *)
  
  (float_of_int a,float_of_int b)
;;

let int_of_float_2 (a,b) =

  (* point -> int * int *)

  (* Fonction int_of_float pour un couple *)
  
  (int_of_float' a,int_of_float' b)
;;

let somme_R2 (x1,y1) (x2,y2) =

  (* point -> point -> point *)

  (* Somme de deux points du plan *)
  
  x1+.x2,y1+.y2
;;

let prod_R2 k (x,y) =

  (* float -> point -> point *)

  (* Multiplication par un scalaire dans le plan *)
  
  (k*.x,k*.y)
;;

let diff_R2 a b =

  (* point -> point -> point *)

  (* Différence de deux points du plan *)
  
  somme_R2 a (prod_R2 (-1.) b)
;;


let init_graph () =

  (* unit -> unit *)

  (* Initialise la sortie graphique *)
  
  moveto 0 0 ;
  clear_graph() ;
  set_color black
;;

let rec applique f = function

  (* ('a -> 'a -> 'a) -> 'a list -> 'a *)

  (* Applique une fonction prenant deux arguments à une liste *)

  |[e]  -> e
  |t::q -> f t (applique f q)
  |[]   -> failwith "cannot_apply_to_empty_list"
;;

let min_list =

  (* 'a list -> 'a *)

  (* Renvoie le minimum d'une liste, avec une complexité linéaire (fonction polymorphe) *)

  applique min
;;

let max_list =

  (* 'a list -> 'a *)

  (* Renvoie le maximum d'une liste, avec une complexité linéaire (fonction polymorphe) *)

  applique max
;;

let list_of_string mot =

  (* string -> char list *)

  (* Décompose une chaîne de caractères en la liste de ses caractères *)

  
  let res = ref [] in
  for i = string_length mot - 1 downto 0 do
    res := mot.[i] :: !res
  done;
  !res
;;

let rec bar = function

  (* (float * float) list -> float *)

  (* Renvoie le barycentre des réels dont le poids est la deuxième composante du couple
     Suppose que la somme des poids est égale à 1 *)

  |[]       -> 0.
  |(p,t)::q -> p *. t +. bar q
;;

let bar2 l =

  (* (point * float) list -> point *)

  (* Renvoie le barycentre des points du plan dont le poids est la deuxième composante du couple
     Suppose que la somme des poids est égale à 1 *)
  
  bar (map (fun ((x,_),t) -> x,t) l),bar (map (fun ((_,y),t) -> y,t) l)
;;

let scinder (p000,p001,p011,p111) t =

  (* point * point * point * point -> float -> (point * point * point * point) * (point * point * point * point) *)

  (* À partir d'un quadruplet de points de contrôle d'une courbe de Bézier, 
     renvoie deux tels quadruplets dont les points calculés par l'algorithme de De Casteljau définissent deux sous-courbes *)
  
  let u = 1. -. t in
  let p00t,p0t1,pt11 = bar2 [(p000,t);(p001,u)],bar2 [(p001,t);(p011,u)],bar2 [(p011,t);(p111,u)] in
  let p0tt,ptt1 = bar2 [(p00t,t);(p0t1,u)],bar2 [(p0t1,t);(pt11,u)] in
  let pttt = bar2 [(p0tt,t);(ptt1,u)] in
  (p000,p00t,p0tt,pttt),(pttt,ptt1,pt11,p111)
;;

let arret (p000,p001,p011,p111) t e =

  (* point * point * point * point -> float -> float -> bool *)

  (* Condition d'arrêt pour l'algorithme de De Casteljau 
     Vrai lorsque tous les points de contrôle sont à une distance environ inférieure à e les uns des autres *)
  
  let pttt = bar2 [(p000,pow_fl t 3);(p001,3. *. pow_fl t 2 *. (1. -. t));(p011,3. *. t *. pow_fl (1. -. t) 2);(p111,pow_fl (1. -. t) 3)] in
  distance1 p000 pttt < e && distance1 p001 pttt < e && distance1 p011 pttt < e && distance1 p111 pttt < e
;;

let attendre t =

  (* float -> unit *)
  
  (* Arrête le processus en cours pour une durée t, en secondes *)

  let heureDebut= sys__time() in
  while sys__time() < (heureDebut +.t) do
    ()
  done
;;
