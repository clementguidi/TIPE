let rec pointsBezier' t pas (x0,y0) (x1,y1) (x2,y2) (x3,y3) =

  (* float -> float -> point -> point -> point -> point -> point list *)
  
  (* Renvoie les points (couples d'entiers) de la courbe paramétrée exacte, pour le pas pas.
     Les couples sont les points de contrôle, et t une variable auxiliaire, initialisée à 0 et allant jusqu'à 1 *)
  
  if t > 1. then [x3,y3] (* Cas d'arrêt : on renvoie le dernier point de contrôle *)
  else let u = 1. -. t in (* Sinon on utilise les équations paramétrées *)
       let x = x0 *. pow_fl u 3 +. 3. *. x1 *. t *. pow_fl u 2 +. 3. *. x2 *. pow_fl t 2 *. u +. pow_fl t 3 *.x3
       and y =y0 *. pow_fl u 3 +. 3. *.y1 *. t *. pow_fl u 2 +. 3. *.y2 *. pow_fl t 2 *. u +. pow_fl t 3 *.y3 in
       (x,y) :: pointsBezier' (t+.pas) pas (x0,y0) (x1,y1) (x2,y2) (x3,y3)
;;

let pointsBezier =

  (* float -> point -> point -> point -> point -> point list *)

  (* Renvoie les points (couples d'entiers) de la courbe paramétrée exacte, pour le pas pas *)
  
  pointsBezier' 0.
;;

let rec casteljau' e  = function

  (* float -> (point * point * point * point) list -> (point * point * point * point) list *)

  (* Renvoie la liste de quadruplets des points de contrôle des sous-courbes calculées par l'algorithme de De Casteljau *)

  |[]                      -> []
  |t::q when arret t 0.5 e -> t :: casteljau' e q
  |t::q                    -> casteljau' e (t1::t2::q) where t1,t2 = scinder t 0.5
;;

let casteljau x =

  (* point * point * point * point -> (point * point * point * point) list *)

  (* Application partielle de casteljau' pour un pas de 1 et les quatre premiers points de contrôle *)

  casteljau' 1. [x]
;;

let pointsCasteljau p q r s =

  (* point -> point -> point -> point -> point list *)

  (* Sélectionne, parmi les points de contrôle fournis par la fonction casteljau, ceux présents sur la courbe *)

  (fst_4 (hd l))::(map fth_4 l) where l = casteljau (p,q,r,s)
;;

let rec traceCourbe' = function

  (* point list -> unit *)

  (* Relie les points de la liste en argument *)

  |[]       -> ()
  |(x,y)::q -> lineto (int_of_float' x) (int_of_float' y) ; traceCourbe' q
;;

let traceCourbe ((x0,y0)::l) =

  (* point list -> unit *)

  (* Relie les points de la liste en argument en se déplaçant préalablement au premier *)
  
  moveto (int_of_float' x0) (int_of_float' y0) ; traceCourbe' ((x0,y0)::l)
;;

let rmoveto x0 y0 x y =

  (* int -> int -> int -> int -> unit *)

  (* Implémentation de l'analogue PostScript *)
  
  moveto (x0+x) (y0+y)
;;

let hmoveto x0 y0 x =

  (* int -> int -> int -> unit *)

  (* Implémentation de l'analogue PostScript *)
  
  rmoveto x0 y0 x 0
;;

let vmoveto x0 y0 =

  (* int -> int -> int -> unit *)

  (* Implémentation de l'analogue PostScript *)
  
  rmoveto x0 y0 0
;;

let rlineto x0 y0 x y =

  (* int -> int -> int -> int -> unit *)

  (* Implémentation de l'analogue PostScript *)
  
  lineto (x0+x) (y0+y)
;;

let vlineto x0 y0 =

  (* int -> int -> int -> unit *)

  (* Implémentation de l'analogue PostScript *)
  
  rlineto x0 y0 0
;;

let hlineto x0 y0 x =

  (* int -> int -> int  -> unit *)

  (* Implémentation de l'analogue PostScript *)
  
  rlineto x0 y0 x 0
;;

let  moveto_fl x  y =

  (* float -> float -> unit *)

  (* Implémentation de l'analogue PostScript, pour les flottants *)
  
  moveto (int_of_float' x) (int_of_float' y)
;;

let rmoveto_fl x0 y0 x y =
  
  (* float -> float -> float -> float -> unit *)

  (* Implémentation de l'analogue PostScript, pour les flottants *)
  
  moveto_fl (x0+.x) (y0+.y)
;;

let hmoveto_fl x0 y0 x =

  (* float -> float -> float -> unit *)

  (* Implémentation de l'analogue PostScript, pour les flottants *)
  
  rmoveto_fl x0 y0 x 0.
;;

let vmoveto_fl x0 y0 =

  (* float -> float -> float -> unit *)

  (* Implémentation de l'analogue PostScript, pour les flottants *)
  
  rmoveto_fl x0 y0 0.
;;

let  lineto_fl x  y =

  (* float -> float -> unit *)

  (* Implémentation de l'analogue PostScript, pour les flottants *)
  
  lineto (int_of_float' x) (int_of_float' y)
;;

let rlineto_fl x0 y0 x y =

  (* float -> float -> float -> float -> unit *)

  (* Implémentation de l'analogue PostScript, pour les flottants *)
  
  lineto_fl (x0+.x) (y0+.y)
;;

let vlineto_fl x0 y0 =

  (* float -> float -> float -> unit *)

  (* Implémentation de l'analogue PostScript, pour les flottants *)
  
  rlineto_fl x0 y0 0.
;;

let hlineto_fl x0 y0 x =

  (* float -> float -> float -> unit *)

  (* Implémentation de l'analogue PostScript, pour les flottants *)
  
  rlineto_fl x0 y0 x 0.
;;

let curveto x0 y0 x1 y1 x2 y2 x3 y3 =

  (* float -> float -> float -> float -> float -> float -> float -> float -> unit *)

  (* Implémentation de l'analogue PostScript *)
  
  traceCourbe (pointsCasteljau (x0,y0) (x1,y1) (x2,y2) (x3,y3))
;;

let rcurveto x0 y0 x1 y1 x2 y2 x3 y3 =

  (* float -> float -> float -> float -> float -> float -> float -> float -> unit *)

  (* Implémentation de l'analogue PostScript *)
  
  curveto x0 y0 (x0+.x1) (y0+.y1) (x0+.x2) (y0+.y2) (x0+.x3) (y0+.y3)
;;

let rrcurveto x0 y0 dx1 dy1 dx2 dy2 dx3 dy3 =

  (* float -> float -> float -> float -> float -> float -> float -> float -> unit *)

  (* Implémentation de l'analogue PostScript *)
  
  rcurveto x0 y0 dx1 dy1 (dx1+.dx2) (dy1+.dy2) (dx1+.dx2+.dx3) (dy1+.dy2+.dy3)
;;

let curveto_B x0 y0 x1 y1 x2 y2 x3 y3 =

  (* float -> float -> float -> float -> float -> float -> float -> float -> unit *)

  (* Implémentation de l'analogue PostScript, par le calcul naïf *)
  
  traceCourbe (pointsBezier 0.001 (x0,y0) (x1,y1) (x2,y2) (x3,y3))
;;

let rcurveto_B x0 y0 x1 y1 x2 y2 x3 y3 =

  (* float -> float -> float -> float -> float -> float -> float -> float -> unit *)

  (* Implémentation de l'analogue PostScript, par le calcul naïf *)
  
  curveto_B x0 y0 (x0+.x1) (y0+.y1) (x0+.x2) (y0+.y2) (x0+.x3) (y0+.y3)
;;

let rrcurveto_B x0 y0 dx1 dy1 dx2 dy2 dx3 dy3 =

  (* float -> float -> float -> float -> float -> float -> float -> float -> unit *)

  (* Implémentation de l'analogue PostScript, par le calcul naïf *)
  
  rcurveto_B x0 y0 dx1 dy1 (dx1+.dx2) (dy1+.dy2) (dx1+.dx2+.dx3) (dy1+.dy2+.dy3)
;;

let rec traceGlyphe' orig act = function

  (* point -> point -> definition -> unit *)

  (* Fontion auxiliaire de traceGlyphe *)

  |[]                                 -> ()
  |(Rmoveto,[x;y])                ::q -> rmoveto_fl x0 y0 x y ;
    traceGlyphe' nouv nouv q where nouv = somme_R2 act (x,y) and x0,y0 = act
  |(Rlineto,[x;y])                ::q -> rlineto_fl x0 y0 x y ;
    traceGlyphe' orig (somme_R2 act (x,y)) q where x0,y0 = act
  |(Vlineto,[y]  )                ::q -> vlineto_fl x0 y0 y ;
    traceGlyphe' orig (somme_R2 act (0.,y)) q where x0,y0 = act
  |(Hlineto,[x]  )                ::q -> hlineto_fl x0 y0 x ;
    traceGlyphe' orig (somme_R2 act (x,0.)) q where x0,y0 = act
  |(Rrcurveto,[x1;y1;x2;y2;x3;y3])::q -> rrcurveto x0 y0 x1 y1 x2 y2 x3 y3 ;
    traceGlyphe' orig (x0+.x1+.x2+.x3,y0+.y1+.y2+.y3) q where x0,y0 = act
  |(Vhcurveto,[y1;x2;y2;x3]      )::q -> rrcurveto x0 y0 0. y1 x2 y2 x3 0. ;
    traceGlyphe' orig (x0+.x2+.x3   ,y0+.y1+.y2   ) q where x0,y0 = act
  |(Hvcurveto,[x1;x2;y2;y3]      )::q -> rrcurveto x0 y0 x1 0. x2 y2 0. y3 ;
    traceGlyphe' orig (x0+.x1+.x2   ,y0+.y2+.y3   ) q where x0,y0 = act
  |(Vmoveto,[y])                  ::q -> vmoveto_fl x0 y0 y ;
    traceGlyphe' nouv nouv q where nouv = somme_R2 act (0.,y) and x0,y0 = act
  |(Hmoveto,[x])                  ::q -> hmoveto_fl x0 y0 x ;
    traceGlyphe' nouv nouv q where nouv = somme_R2 act (x,0.) and x0,y0 = act
  |(Closepath,_)                  ::q -> lineto_fl x0 y0 ;
    traceGlyphe' orig act q where x0,y0 = orig
  |(Endchar,_)                    ::q -> lineto_fl x0 y0 ;
    traceGlyphe' orig act q where x0,y0 = orig
  |_                                    -> failwith "instruction_de_trace_non_reconnue" 
;;

let traceGlyphe x =

  (* point -> definition -> unit *)

  (* Trace un glyphes à un point x donné à partir de sa définition *)

  traceGlyphe' x x
;;

let homothetie k =

  (* float -> definition -> definition *)

  (* Applique l'homothétie de rapport k à la définition d'un glyphe *)
  
  map (fun (i,l) -> i,map (fun x -> x *. k) l)
;;

let rotation' angle (x,y) =

  (* float -> point -> point *)

  (* Applique la rotation d'angle angle autour du point (0,0) à (x,y) *)

  cos angle *. x +. sin angle *. y, -. sin angle *.x +. cos angle *. y
;;

let rec rotation a = function

  (* float -> definition -> definition *)

  (* Applique la rotation d'angle angle autour du point (0,0) à la définition d'un glyphe *)

  |[]                                 -> []
  |(Rmoveto,[x;y])                ::q -> (Rmoveto,[x';y']) :: rotation a q where x',y' = rotation' a  (x,y)
  |(Rlineto,[x;y])                ::q -> (Rlineto,[x';y']) :: rotation a q where x',y' = rotation' a  (x,y)
  |(Vlineto,[y]  )                ::q -> (Rlineto,[x';y']) :: rotation a q where x',y' = rotation' a  (0.,y)
  |(Hlineto,[x]  )                ::q -> (Rlineto,[x';y']) :: rotation a q where x',y' = rotation' a  (x,0.)
  |(Rrcurveto,[x1;y1;x2;y2;x3;y3])::q -> (Rrcurveto,[x1';y1';x2';y2';x3';y3']) :: rotation a q
    where x1',y1' = rotation' a  (x1,y1) and x2',y2' = rotation' a  (x2,y2) and x3',y3' = rotation' a  (x3,y3)
  |(Vhcurveto,[y1;x2;y2;x3]      )::q -> rotation a ((Rrcurveto,[0.;y1;x2;y2;x3;0.])::q)
  |(Hvcurveto,[x1;x2;y2;y3]      )::q -> rotation a ((Rrcurveto,[x1;0.;x2;y2;0.;y3])::q)
  |(Vmoveto,[y]  )                ::q -> (Rmoveto,[x';y']) :: rotation a q where x',y' = rotation' a  (0.,y)
  |(Hmoveto,[x]  )                ::q -> (Rmoveto,[x';y']) :: rotation a q where x',y' = rotation' a  (x,0.)
  |t                                ::q -> t :: rotation a q  
;;

let traceBoundingBox p ((xmin,ymin),(xmax,ymax)) =

  (* boundingBox -> unit *)

  (* Trace une bounding box *)
  
  traceCourbe (map (fun q -> somme_R2 p q) [xmin,ymin;xmin,ymax;xmax,ymax;xmax,ymin;xmin,ymin])
;;

let rec arrivee pos = function

  (* point -> instruction -> point *)

  (* Donne la position d'arrivée après l'application d'une instruction, connaissant la position initiale *)

  |(Rlineto,[x;y])                 -> somme_R2 pos (x,y)
  |(Vlineto,[y]  )                 -> arrivee pos (Rlineto,[0.;y])
  |(Hlineto,[x]  )                 -> arrivee pos (Rlineto,[x;0.])
  |(Rrcurveto,[x1;y1;x2;y2;x3;y3]) -> somme_R2 pos (x1+.x2+.x3,y1+.y2+.y3)
  |(Vhcurveto,[y1;x2;y2;x3]      ) -> arrivee pos (Rrcurveto,[0.;y1;x2;y2;x3;0.])
  |(Hvcurveto,[x1;x2;y2;y3]      ) -> arrivee pos (Rrcurveto,[x1;0.;x2;y2;0.;y3])
  |_                               -> failwith "instruction_de_tracé_non_reconnue"
;;

let rec traceBoundingBoxesIndividuelles' orig pos couleurs = function

  (* point -> point -> color list -> definition -> unit *)

  (* Fonction auxiliaire de traceBoundingBoxesIndividuelles *)

  |[]                                 -> ()
  |(Rmoveto,[x;y])                ::q -> traceBoundingBoxesIndividuelles' nouv nouv couleurs q where nouv = (somme_R2 pos (x,y))
  |(Hmoveto,[x])                  ::q -> traceBoundingBoxesIndividuelles' orig pos couleurs ((Rmoveto,[x;0.])::q)
  |(Vmoveto,[y])                  ::q -> traceBoundingBoxesIndividuelles' orig pos couleurs ((Rmoveto,[0.;y])::q)
  |(Closepath,_)                  ::q -> 
    set_color (hd couleurs) ;
    traceBoundingBox pos (boundingBoxExacte (0.,0.) (Rlineto,[x;y])) ;
    traceBoundingBoxesIndividuelles' orig pos ((tl couleurs) @ [hd couleurs]) q
      where x,y = diff_R2 orig pos
  |(Endchar,_)                    ::q ->
    set_color (hd couleurs) ;
    traceBoundingBox pos (boundingBoxExacte (0.,0.) (Rlineto,[x;y])) ;
    traceBoundingBoxesIndividuelles' orig pos ((tl couleurs) @ [hd couleurs]) q
      where x,y = diff_R2 orig pos
  |instruction                    ::q ->
    set_color (hd couleurs) ;
    traceBoundingBox pos (boundingBoxExacte (0.,0.) instruction) ;
    traceBoundingBoxesIndividuelles' orig (arrivee pos instruction) ((tl couleurs) @ [hd couleurs]) q
;;

let traceBoundingBoxesIndividuelles p =

  (* point -> definition -> unit *)

  (* Trace les bounding boxes relatives à chacune des parties d'un glyphe *)

  traceBoundingBoxesIndividuelles' p p [red;green;blue;yellow;cyan;magenta]
;;

let rec traceBoundingBoxPasAPas' p couleurs = function

  (* point -> color list -> definition -> unit *)

  (* Fonction auxiliaire de traceBoundingBoxPasAPas *)

  |[]   -> ()
  |t::q ->
    traceBoundingBoxPasAPas' p ((tl couleurs) @ [hd couleurs]) q ;
    set_color (hd couleurs) ;
    attendre 2. ;
    traceBoundingBox p (boundingBox (rev (t::q)))
;;

let traceBoundingBoxPasAPas p glyphe =

  (* point -> definition -> unit *)

  (* Trace les différentes bounding boxes résultant des fusions successives lors du calcul de la bounding box d'un glyphe, à partir du point p *)
  
  traceBoundingBoxPasAPas' p [red;green;blue;yellow;cyan;magenta] (rev (snd glyphe))
;;

let rec ecrireMot' fonte pos k a = function

  (* definition -> point -> float -> float -> char list -> string *)

  (* Fonction auxiliaire de ecrireMot *)

  |[]     -> ""
  |` `::q ->
    let l = (largeur (assoc `x` fonte)) *. k in
    let def = homothetie k (rotation a ([Hmoveto,[l]])) in
    traceGlyphe pos def ;
    " " ^ ecrireMot' fonte (somme_R2 pos (cos a *. l,-. sin a *. l)) k a q
  |car::q ->
    let l = (largeur (assoc car fonte)) *. k *. 1.1 in
    let def = homothetie k (rotation a (assoc car fonte)) in
    traceGlyphe pos def ;
    string_of_char car ^ ecrireMot' fonte (somme_R2 pos (cos a *. l,-. sin a *. l)) k a q
;;

let ecrireMot fonte pos k a mot =

  (* definition -> point -> float -> float -> string -> string *)

  (* Écrit, à partir du point pos, le mot mot, avec la fonte fonte, en appliquant l'homothétie de rapport k et la rotation d'angle a *)
  
  ecrireMot' fonte pos k a (list_of_string mot)
;;

let traceQuadrillage () =

  (* unit -> unit *)

  (* Trace un quadrillage adapté à une résolution 1600x900 *)
  
  set_color (rgb 200 200 200) ;
  for i = 0 to 20 do
    moveto (72*i) 0 ;
    rlineto (72*i) 0 0 10000 ;
    moveto 0 (72*i) ;
    rlineto 0 (72*i) 10000 0
  done;
  set_color black
;;
