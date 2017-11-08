let rec boundingBoxGrossiere (x0,y0) = function

  (* point_fl -> instruction_fl -> boundingBox_fl *)

  (* Renvoie la bounding box des points de contrôle d'une instruction de tracée donnée *)

  (*|commande,arguments -> ((xmin,ymin),(xmax,ymax))*)
  |Rlineto,[dx;dy]                     ->
    (min x0 (x0+.dx),min y0 (y0+.dy)),(max x0 (x0+.dx),max y0 (y0+.dy))
  |Vlineto,[dy]                        ->
    boundingBoxGrossiere (x0,y0) (Rlineto,[0.;dy])
  |Hlineto,[dx]                        ->
    boundingBoxGrossiere (x0,y0) (Rlineto,[dx;0.])
  |Rrcurveto,[dx1;dy1;dx2;dy2;dx3;dy3] ->
    (min_list [x0;x0+.dx1;x0+.dx1+.dx2;x0+.dx1+.dx2+.dx3],
     min_list [y0;y0+.dy1;y0+.dy1+.dy2;y0+.dy1+.dy2+.dy3]),
    (max_list [x0;x0+.dx1;x0+.dx1+.dx2;x0+.dx1+.dx2+.dx3],
     max_list [y0;y0+.dy1;y0+.dy1+.dy2;y0+.dy1+.dy2+.dy3])
  |_ -> failwith "cas_non_envisagé"
;;

let incluseBoundingBoxp ((xmin1,ymin1),(xmax1,ymax1)) ((xmin2,ymin2),(xmax2,ymax2)) =

  (* boundingBox -> boundingBox -> bool *)

  (* Vrai lorsque la première bounding box est incluse dans la seconde *)
  
  xmin1 > xmin2 && ymin1 > ymin2 && xmax1 < xmax2 && ymax1 < ymax2
;;

let mergeBoundingBoxes ((xmin1,ymin1),(xmax1,ymax1)) ((xmin2,ymin2),(xmax2,ymax2)) =

  (* boundingBox -> boundingBox -> boundingBox *)

  (* Fusionne deux bounding box dans une bounding box plus grande *)
  
  (min xmin1 xmin2,min ymin1 ymin2),(max xmax1 xmax2,max ymax1 ymax2)
;;

let min_list = applique min;; (* redéfinitions pour comptabilité *)
let max_list = applique max;;

let rec boundingBoxBezier p000 p001 p011 p111 =

  (* point_fl -> point_fl -> point_fl -> point_fl -> boundingBox_fl *)

  (* Renvoie la bounding box d'une cubique de bézier.
     Les extrema sont recherchés par dichotomie, en utilisant le principe de l'algorithme de De Casteljau *)
  
  if arret (p000,p001,p011,p111) 0.5 1.
  then ((min_list [x0;x1;x2;x3],
	min_list [y0;y1;y2;y3]),(max_list [x0;x1;x2;x3],
				 max_list [y0;y1;y2;y3]) where (x0,y0),(x1,y1),(x2,y2),(x3,y3)=p000,p001,p011,p111)
  else mergeBoundingBoxes (boundingBoxBezier p000 p00t p0tt pttt) (boundingBoxBezier pttt ptt1 pt11 p111)
    where (p000,p00t,p0tt,pttt),(_,ptt1,pt11,p111) = scinder (p000,p001,p011,p111) 0.5
;;

let rec boundingBoxExacte (x0,y0) = function

  (* point_fl -> instruction_fl -> boundingBox_fl *)

  (* Renvoie la bounding box de la courbe dont l'instruction de tracé est donnée en argument *)

  (*|commande,arguments -> ((xmin,ymin),(xmax,ymax))*)
  |Rrcurveto,[dx1;dy1;dx2;dy2;dx3;dy3] ->
    boundingBoxBezier
      (x0,y0)
      (x0+.dx1,y0+.dy1)
      (x0+.dx1+.dx2,y0+.dy1+.dy2)
      (x0+.dx1+.dx2+.dx3,y0+.dy1+.dy2+.dy3)
  |Vhcurveto,[dy1;dx2;dy2;dx3]         ->
    boundingBoxExacte (x0,y0) (Rrcurveto,[0.;dy1;dx2;dy2;dx3;0.])
  |Hvcurveto,[dx1;dx2;dy2;dy3]         ->
    boundingBoxExacte (x0,y0) (Rrcurveto,[dx1;0.;dx2;dy2;0.;dy3])
  |instruction                         ->
    boundingBoxGrossiere (x0,y0) instruction                             
;;

let rec boundingBox' orig act box = function

  (* point_fl -> point_fl -> boundingBox_fl -> boundingBox_fl *)

  (* Fonction auxiliaire de boundingBox *)

  |[]                                 ->
    box
  |(Rmoveto,[x;y])                ::q ->
    boundingBox' nouv nouv box q where nouv = somme_R2 act (x,y)
  |(Rlineto,[x;y])                ::q ->
    boundingBox' orig (somme_R2 act (x,y)) (mergeBoundingBoxes box (boundingBoxExacte act (Rlineto,[x;y]))) q
  |(Vlineto,[y]  )                ::q ->
    boundingBox' orig act box ((Rlineto,[0.;y])::q)
  |(Hlineto,[x]  )                ::q ->
    boundingBox' orig act box ((Rlineto,[x;0.])::q)
  |(Rrcurveto,[x1;y1;x2;y2;x3;y3])::q ->
    if incluseBoundingBoxp (boundingBoxGrossiere act (Rrcurveto,[x1;y1;x2;y2;x3;y3])) box
    then (boundingBox' orig (x0+.x1+.x2+.x3,y0+.y1+.y2+.y3) box q)
    else boundingBox' orig (x0+.x1+.x2+.x3,y0+.y1+.y2+.y3) (mergeBoundingBoxes box (boundingBoxExacte act (Rrcurveto,[x1;y1;x2;y2;x3;y3]))) q
      where x0,y0 = act
  |(Vhcurveto,[y1;x2;y2;x3]      )::q ->
    boundingBox' orig act box ((Rrcurveto,[0.;y1;x2;y2;x3;0.])::q)
  |(Hvcurveto,[x1;x2;y2;y3]      )::q ->
    boundingBox' orig act box ((Rrcurveto,[x1;0.;x2;y2;0.;y3])::q)
  |(Vmoveto,[y])                  ::q ->
    boundingBox' orig act box ((Rmoveto,[0.;y])::q)
  |(Hmoveto,[x])                  ::q ->
    boundingBox' orig act box ((Rmoveto,[x;0.])::q)
  |(Closepath,_)                  ::q ->
    boundingBox' orig act (mergeBoundingBoxes box (boundingBoxExacte act (Rlineto,[x;y]))) q
    where x,y = diff_R2 orig act
  |(Endchar,_)                    ::q ->
    boundingBox' orig act (mergeBoundingBoxes box (boundingBoxExacte act (Rlineto,[x;y]))) q
    where x,y = diff_R2 orig act
  |_                                  ->
    failwith "instruction_de_trace_non_reconnue" 
;;

let initialiseBoundingBox = function

  (* definition -> boundingBox *)

  (* Renvoie une bounding box nulle comprise dans la bounding box du glyphe *)

  |(Rmoveto,[x;y])::_ -> (x,y),(x,y)
  |(Hmoveto,[x])::_   -> (x,0.),(x,0.)
  |(Vmoveto,[y])::_   -> (0.,y),(0.,y)
  |_                  -> failwith "définition_ne_commence_pas_par_un_déplacement_du_point_courant"
;;

let boundingBox definition =

  (* definition -> boundingBox *)

  (* Renvoie la bounding box d'un glyphe *)
  
  boundingBox' (0.,0.) (0.,0.) bb0 definition where bb0 = initialiseBoundingBox definition
;; 

let largeur def =

  (* definition -> int *)

  (* Renvoie la largeur d'un glyphe *)

  xmax -. xmin where (xmin,_),(xmax,_) = boundingBox def
;; 
let hauteur def =

  (* definition -> int *)

  (* Renvoie la hauteur d'un glyphe *)

  ymax -. ymin where (_,ymin),(_,ymax) = boundingBox def ;;

let hauteurX fonte =

  (* fonte -> int *)

  (* Renvoie la hauteur d'x pour une fonte *)
  
  hauteur (assoc `x` fonte)
;;

let rec boundingBoxPasAPas' = function (* Pour export vers Ocaml *)
  |[e]   -> [boundingBox [e]]
  |t::q -> boundingBox (rev (t::q)) :: (boundingBoxPasAPas' q)
  |_    -> failwith "bounding_box_incalculable"
;;

let boundingBoxPasAPas definition = (* Pour export vers Ocaml *)
  let bb = boundingBoxPasAPas' (rev definition) in
  let posMin = fst (hd bb) in
  map (fun (m,M) -> int_of_float_2 (diff_R2 m posMin),int_of_float_2 (diff_R2 M posMin)) bb
;;
