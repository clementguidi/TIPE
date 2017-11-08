let rec eqParam (x0,y0) (cmd,args) =

  (* point -> instruction -> (float -> float) * (float -> float) * point) *)
  
  (*Renvoie le couple d'équations paramétrées de la courbe correspondant à l'instruction PostScript, et le point d'arrivée ; (a0,b0) est le point de départ *)
  
  match cmd,args with
  |Rlineto,[x;y]                 ->
    (fun t -> x0 +. x *. t),(fun t -> y0 +. y *. t),(x0+.x,y0+.y)
  |Hlineto,[x]                   -> eqParam (x0,y0) (Rlineto,[x;0.])
  |Vlineto,[y]                   -> eqParam (x0,y0) (Rlineto,[0.;y])
  |Rrcurveto,[x1;y1;x2;y2;x3;y3] ->
    let a0,b0 = x0,y0 in
    let a1,b1 = a0+.x1,b0+.y1 in
    let a2,b2 = a1+.x2,b1+.y2 in
    let a3,b3 = a2+.x3,b2+.y3 in
    (fun t -> a0*.pow_fl (1.-.t) 3+.3.*.a1*.t*.pow_fl (1.-.t) 2+.3.*.a2*.pow_fl t 2*.(1.-.t)+.a3*.pow_fl t 3),
    (fun t -> b0*.pow_fl (1.-.t) 3+.3.*.b1*.t*.pow_fl (1.-.t) 2+.3.*.b2*.pow_fl t 2*.(1.-.t)+.b3*.pow_fl t 3),
    (x0+.x1+.x2+.x3,y0+.y1+.y2+.y3)
  |_                             -> (fun _ -> 0.),(fun _ -> 0.),(x0,y0)
;;

let rectanglesGauche f a b n =

  (* (float -> float) -> float -> float -> int -> float *)

  (* Intégration numérique par la méthode des rectangles à gauche *)
  
  let h = (b -. a) /. float_of_int n in
  let res = ref 0. in
  for i = 0 to n-1 do
    res := !res +. f (a +. float_of_int i *. h);
  done;
  h *. !res
;;

let simpson f a b n =

  (* (float -> float) -> float -> float -> int -> float *)

  (* Intégration numérique par la méthode de Simpson *)
  
  let h = (b -. a) /. float_of_int n in
  let res = ref 0. in
  for i = 1 to n-1 do
    res := !res +. f (a +. float_of_int i *. h) +. 2. *. f (a +. (float_of_int i +. 0.5) *. h)
  done;
  res := !res +. 2. *. (f a +. f b);
  h *. !res /. 3.
;;

let derive f h t =

  (* (float -> float) -> float -> float -> float *)

  (* Dérivée numérique *)
  
  (f (t +. h) -. f (t -. h)) /. 2. /. h
;;

let aireCubique (x0,y0) (x1,y1) (x2,y2) (x3,y3) =

  (* point -> point -> point -> point -> float *)

  (* Renvoie l'aire sous une cubique de Bézier, en utilisant la méthode de Simpson *)
  
  let n = 10 in
  let h = 1. /. float_of_int n in
  let x t =
    let u = 1. -. t in
    pow_fl u 3 *.x0 +. 3. *. t *. pow_fl u 2 *.x1 +. 3. *. pow_fl t 2 *. u *.x2 +. pow_fl t 3 *.x3
  in
  let y t =
    let u = 1. -. t in
    pow_fl u 3 *.y0 +. 3. *. t *. pow_fl u 2 *.y1 +. 3. *. pow_fl t 2 *. u *.y2 +. pow_fl t 3 *.y3
  in
  simpson (fun t -> (x t *. (derive y h t)) -. y t *. (derive x h t)) 0. 1. n /. 2.
;;

let rec aireGlyphe' orig pos = function

  (* point -> point -> definition -> float *)

  (* Fonction auxiliaire de aireGlyphe *)

  |[]                                 -> 0.
  |(Rmoveto,[x;y])                ::q -> aireGlyphe' nouv nouv q where nouv = somme_R2 pos (x,y)
  |(Rlineto,[x;y])                ::q -> (x0*.y-.y0*.x)/.2. +. aireGlyphe' orig (somme_R2 pos (x,y)) q where x0,y0 = pos
  |(Vlineto,[y]  )                ::q -> aireGlyphe' orig pos ((Rlineto,[0.;y])::q)
  |(Hlineto,[x]  )                ::q -> aireGlyphe' orig pos ((Rlineto,[x;0.])::q)
  |(Rrcurveto,[x1;y1;x2;y2;x3;y3])::q ->
    let a1,a2 = pos in
    let b1,b2 = somme_R2 (a1,a2) (x1,y1) in
    let c1,c2 = somme_R2 (b1,b2) (x2,y2) in
    let d1,d2 = somme_R2 (c1,c2) (x3,y3) in
    ((6.*.c1+.3.*.b1+.a1)*.d2
     +.(-.6.*.c2-.3.*.b2-.a2)*.d1
     +.(3.*.b1+.3.*.a1)*.c2
     +.(-.3.*.b2-.3.*.a2)*.c1
     +.6.*.a1*.b2-.6.*.a2*.b1)
    /.20.
    +. aireGlyphe' orig (d1,d2) q
  |(Vhcurveto,[y1;x2;y2;x3]      )::q -> aireGlyphe' orig pos ((Rrcurveto,[0.;y1;x2;y2;x3;0.])::q)
  |(Hvcurveto,[x1;x2;y2;y3]      )::q -> aireGlyphe' orig pos ((Rrcurveto,[x1;0.;x2;y2;0.;y3])::q)
  |(Vmoveto,[y]  )                ::q -> aireGlyphe' orig pos ((Rmoveto,[0.;y])::q)
  |(Hmoveto,[x]  )                ::q -> aireGlyphe' orig pos ((Rmoveto,[x;0.])::q)
  |(Closepath,_  )                ::q -> (x0*.y-.y0*.x)/.2. +. aireGlyphe' orig pos q
    where x0,y0 = orig and x,y = diff_R2 orig pos
  |_                              ::q -> aireGlyphe' orig pos q
;;

let aireGlyphe =

  (* definition -> float *)

  (* Calcule l'aire d'un glyphe d'après sa définition *)
  
  aireGlyphe' (0.,0.) (0.,0.)
;;

let airesPonderees fonte =

  (* fonte -> float *)

  (* Calcule l'aire pondérée moyenne d'un glyphe pour une fonte *)
  
  let freq = frequences "Thèse de Broglie" in
  let aires = make_vect 72 0. in
  for i = 0 to vect_length aires - 1 do
    aires.(i) <- aireGlyphe (assoc (char_of_int' i) fonte)
  done;
  let res = make_vect 72 0. in
  for i = 0 to vect_length aires - 1 do
    res.(i) <- freq.(i) *. aires.(i)
  done;
  (somme_tableau_fl res) /. (pow_fl (hauteurX fonte) 2)
;;
