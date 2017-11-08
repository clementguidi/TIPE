let incremente tab i =

  (* int vect -> int -> unit *)

  (* Incrémente la case i de tab de 1 *)
  
  tab.(i) <- tab.(i) + 1
;;

let int_of_char' = function

  (* char -> int *)

  (* Personnalisation de int_of_char *)

  |` `                        -> 0
  |`!`                        -> 1
  |`,`                        -> 2
  |`.`                        -> 3
  |`?`                        -> 4
  |t when int_of_char t < 39  -> -1
  |t when int_of_char t < 42  -> int_of_char t - 34 (* '() *)
  |t when int_of_char t < 48  -> -1
  |t when int_of_char t < 58  -> int_of_char t - 38 (* 0-9 *)
  |t when int_of_char t < 60  -> int_of_char t - 50 (* :; *)
  |t when int_of_char t < 65  -> -1
  |t when int_of_char t < 91  -> int_of_char t - 45 (* A-Z *)
  |t when int_of_char t < 97  -> -1
  |t when int_of_char t < 123 -> int_of_char t - 51 (* a-z *)
  |_                          -> -1
;;

let char_of_int' = function

  (* int -> char *)

  (* Personnalisation de char_of_int *)

  |0                        -> ` `
  |1                        -> `!`
  |2                        -> `,`
  |3                        -> `.`
  |4                        -> `?`
  |n when n < 8             -> char_of_int (n + 34) (* '() *)
  |n when n < 10            -> char_of_int (n + 50) (* :; *)
  |n when n < 20            -> char_of_int (n + 38) (* 0-9 *)
  |n when n < 46            -> char_of_int (n + 45) (* A-Z *)
  |n when n < 72            -> char_of_int (n + 51) (* a-z *)
  |_                        -> failwith "pas_de_caractere_correspondant"
;;

let rec ajoute tab = function

  (* int vect -> char list -> unit *)

  (* Compte, par effet de bord, les lettres d'une ligne *)

  |[]                            -> ()
  |t::q when int_of_char' t = -1 -> ()
  |t::q                          ->
    incremente tab (int_of_char' t) ;
    ajoute tab q
;;

let rec parcourir tab = function

  (* int vect -> string list -> unit *)

  (* Compte, par effet de bord, les lettres d'un fichier *)

  |[]   -> ()
  |t::q -> ajoute tab (list_of_string t) ; parcourir tab q
;;

let somme_tableau tab =

  (* int vect -> int *)

  (* Somme les composantes d'un vecteur d'entiers *)
  
  let somme = ref 0 in
  for i = 1 to vect_length tab - 1 do
    somme := !somme + tab.(i)
  done;
  !somme
;;

let somme_tableau_fl tab =

  (* float vect -> int *)

  (* Somme les composantes d'un vecteur de flottants *)
  
  let somme = ref 0. in
  for i = 1 to vect_length tab - 1 do
    somme := !somme +. tab.(i)
  done;
  !somme
;;

let frequences fichier =

  (* string -> float vect *)

  (* Calcule la fréquence d'apparition des caractères étudiés dans fichier *)
  
  let tab = make_vect 72 0 in
  let res = make_vect 72 0. in
  let lignes = lit ("../Documents/Livres/" ^ fichier ^ ".txt") in
  parcourir tab lignes ;
  let somme = float_of_int (somme_tableau tab) in
  for i = 1 to vect_length tab - 1 do
    res.(i) <- (float_of_int tab.(i)) /. somme
  done;
  res
;;
