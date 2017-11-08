let lit fichier =

  (* string -> string list *)
  
  (* Renvoie la liste des lignes du fichier fichier *)
  
  let entree = open_in fichier in
  let rec creerListe acc =
    try
      creerListe (input_line entree :: acc)
    with End_of_file -> rev acc
  in
  creerListe []
;;

let split_ligne sep com ligne =

  (* char -> char -> string -> string list *)

  (* Sépare une chaîne de caractères en mots, délimités par sep, en s'arrêtant si le symbole de commentaire com est rencontré. Supprime par ailleurs les tabulations *)
  
  let res = ref [] in
  let mot = ref "" in
  let stop = ref false in
  let i = ref 0 in
  while !i < string_length ligne && not !stop do
    begin match ligne.[!i] with
    |c when c=sep && !mot="" -> ()
    |c when c=sep            -> res := !mot :: !res ; mot := ""
    |c when c=com            -> stop := true
    |c when c=`\t`           -> ()
    |c                       -> mot := !mot ^ string_of_char c
    end ;
    incr i
  done;
  if !mot = "" then rev !res
  else rev (!mot :: !res)
;;

let rec formate_ligne' acc = function

  (* int list -> string list -> string * int list *)

  (* Fonction auxiliaire de formate_liste, la généralisant *)

  |[]   -> "",[]
  |[a]  -> a,rev acc
  |t::q -> formate_ligne' (int_of_string t::acc) q
;;

let formate_ligne =

  (* string list -> string * int list *)

  (* Traite une ligne d'instruction de tracé PostScript, en renvoyant le couple de l'instruction, et de ses arguments, relatifs *)
  
  formate_ligne' []
;;

let recupereGlyphesEtSubrs fichier =
  
  (* string list list -> (char * (string * int list) list) list * (int * (string * int list) list) list *)
  
  (* Regroupe les caractères et les instructions les définissant dans une première liste, puis les sous-routines PostScript éventuelles et les instructions les définissant dans une seconde, à partir d'une liste d'instructions PostScript dont les lignes sont des listes de chaînes *)
  
  let rec traite_ligne car n temp = function
    |[]                             -> [],[]
    |(t::p::q)::l when t = "dup"    -> traite_ligne ` ` (int_of_string p) [] l
    |(t::p::q)::l when p = "NP"     -> glyphes,(n,rev temp)::subrs where glyphes,subrs = traite_ligne ` ` 0 [] l
    |("/space"::p::q)::l            -> traite_ligne ` ` 0 [] l
    |("/exclam"::p::q)::l           -> traite_ligne `!` 0 [] l
    |("/quotesingle"::p::q)::l      -> traite_ligne `'` 0 [] l
    |("/parenleft"::p::q)::l        -> traite_ligne `(` 0 [] l
    |("/parenright"::p::q)::l       -> traite_ligne `)` 0 [] l
    |("/comma"::p::q)::l            -> traite_ligne `,` 0 [] l
    |("/period"::p::q)::l           -> traite_ligne `.` 0 [] l
    |("/colon"::p::q)::l            -> traite_ligne `:` 0 [] l
    |("/semicolon"::p::q)::l        -> traite_ligne `;` 0 [] l
    |("/question"::p::q)::l         -> traite_ligne `?` 0 [] l
    |("/zero"::p::q)::l             -> traite_ligne `0` 0 [] l
    |("/one"::p::q)::l              -> traite_ligne `1` 0 [] l
    |("/two"::p::q)::l              -> traite_ligne `2` 0 [] l
    |("/three"::p::q)::l            -> traite_ligne `3` 0 [] l
    |("/four"::p::q)::l             -> traite_ligne `4` 0 [] l
    |("/five"::p::q)::l             -> traite_ligne `5` 0 [] l
    |("/six"::p::q)::l              -> traite_ligne `6` 0 [] l
    |("/seven"::p::q)::l            -> traite_ligne `7` 0 [] l
    |("/eight"::p::q)::l            -> traite_ligne `8` 0 [] l
    |("/nine"::p::q)::l             -> traite_ligne `9` 0 [] l
    |(t::p::q)::l when t.[0] = `/`  -> traite_ligne t.[1] 0 [] l
    |(t::p::q)::l when p = "ND"     -> (car,rev temp)::glyphes,subrs where glyphes,subrs = traite_ligne ` ` 0 [] l
    |(t::q)::l    when t = "return" -> traite_ligne car n temp l
    |t::l                           -> traite_ligne car n (formate_ligne t :: temp) l
  in
  traite_ligne ` ` 0 [] fichier
;;

let traiteSubr subrs (com,l) = match com with

  (* ('a * (string * 'a list) list) list ->  string * 'a list -> (string * 'a list) list *)
    
  (* Renvoie la liste comprenant l'instruction passée en argument, en la remplaçant par sa description si c'est une sous-routine *)
    
  |"callsubr" -> assoc (hd l) subrs
  |_      -> [com,l]
;;

let remplaceSubrs glyphes subrs =

  (* ('a * (string * 'b list) list) list -> ('b * (string * 'b list) list) list -> ('a * (string * 'b list) list) list *)

  (* Remplace les appels à des sous-routines par leurs descriptions exactes dans la description d'un glyphe *)
  
  map (fun (c,d) -> c,flat_map (traiteSubr subrs) d) glyphes
;;

let instructionReconnuep i =

  (* string list *)
  
  (* Vrai lorsque l'instruction de tracé est reconnue *)

  mem i ["rmoveto";"vmoveto";"hmoveto";"rlineto";"vlineto";"hlineto";"rrcurveto";"vhcurveto";"hvcurveto";"closepath";"endchar"]
;;

let commande_of_string = function

  (* string -> commande *)

  (* Convertit une chaîne de caractères en une commande, de type commande *)

  |"rmoveto"   -> Rmoveto
  |"vmoveto"   -> Vmoveto
  |"hmoveto"   -> Hmoveto
  |"rlineto"   -> Rlineto
  |"vlineto"   -> Vlineto
  |"hlineto"   -> Hlineto
  |"rrcurveto" -> Rrcurveto
  |"vhcurveto" -> Vhcurveto
  |"hvcurveto" -> Hvcurveto
  |"closepath" -> Closepath
  |"endchar"   -> Endchar
  |_           -> failwith "instruction_de_tracé_non_reconnue"
;;    

let rec convertitDefinition = function

  (* (string * int list) list -> definition *)

  (* Convertit une définition vers le type definition en convertissant les chaînes de caractères vers le type commande et les entiers en flottants *)

  |[] -> []
  |(inst,args)::q when instructionReconnuep inst ->
    (commande_of_string inst,map float_of_int args) :: convertitDefinition q
  |_          ::q                                ->
    convertitDefinition q
;;

let convertitGlyphes =

  (* (char * (string * int list) list) list -> fonte *)

  (* Convertit toutes les définitions d'une fonte vers le type definition *)
  
  map (fun (c,d) -> c,convertitDefinition d)
;;

let recupereFonte nom =

  (* string -> fonte *)

  (* Renvoie la description de la fonte dont le nom est donné en argument *)
  
  let lignes = lit ("../Fontes/" ^ nom ^ "/" ^ nom ^ ".asm") in
  let lignes_sep = map (split_ligne ` ` `%`) lignes in
  let glyphes,subrs = recupereGlyphesEtSubrs lignes_sep in
  let glyphes_final = remplaceSubrs glyphes subrs in
  let fonte = convertitGlyphes glyphes_final in
  fonte
;;
