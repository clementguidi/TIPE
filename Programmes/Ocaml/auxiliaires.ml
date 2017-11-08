type commande =
  Rmoveto
| Vmoveto
| Hmoveto
| Rlineto
| Vlineto
| Hlineto
| Rrcurveto
| Vhcurveto
| Hvcurveto
| Closepath
| Endchar

type commande_abs =
  Moveto
| Lineto
| Curveto
|Closepath
| Endchar

type dir =
  Ne
| No
| So
| Se

let brown = rgb 150 70 0

let hd (t::_) = t
let tl (_::q) = q

let rec rev = function
  |[]   -> []
  |t::q -> (rev q) @ [t]
  
let rec int_of_float' x = if x >= 0. then int_of_float (x+.0.5) else - int_of_float' (-.x)
let int_of_float_2 (a,b) = (int_of_float' a,int_of_float' b)
    
let rec map f = function
  |[] -> []
  |t::q -> f t :: map f q

let rec bar = function (* Suppose que la sommes des coefficients est 1 *) (* (float * float) list -> float *)
  |[]       -> 0.
  |(p,t)::q -> p *. t +. bar q

let bar2 l = (* Suppose que la sommes des coefficients est 1 *) (* (point_fl * float) list -> point_fl *)
  bar (map (fun ((x,_),t) -> x,t) l),bar (map (fun ((_,y),t) -> y,t) l)

let scinder (p000,p001,p011,p111) t = (* point_fl * point_fl * point_fl * point_fl -> float -> (point_fl * point_fl * point_fl * point_fl) * (point_fl * point_fl * point_fl * point_fl) *)
  let u = 1. -. t in
  let p00t,p0t1,pt11 = bar2 [(p000,t);(p001,u)],bar2 [(p001,t);(p011,u)],bar2 [(p011,t);(p111,u)] in
  let p0tt,ptt1 = bar2 [(p00t,t);(p0t1,u)],bar2 [(p0t1,t);(pt11,u)] in
  let pttt = bar2 [(p0tt,t);(ptt1,u)] in
  (p000,p00t,p0tt,pttt),(pttt,ptt1,pt11,p111)

let ecrit taille str (x,y) =
  let x0,y0 = current_point () in
  set_text_size taille ;
  moveto x y ;
  draw_string str ;
  moveto x0 y0
    
let affiche_point' taille dir (x,y) nom =
  let r = 2 in
  set_text_size taille ;
  match dir with
  |No -> fill_circle x y r ; ecrit taille nom (x-3-fst (text_size nom),y+3)
  |Ne -> fill_circle x y r ; ecrit taille nom (x+3,y+3)
  |Se -> fill_circle x y r ; ecrit taille nom (x+3,y-3-snd (text_size nom))
  |So -> fill_circle x y r ; ecrit taille nom (x-3-fst (text_size nom),y-3-snd (text_size nom))

let affiche_point taille dir point nom = affiche_point' taille dir (int_of_float_2 point) nom

let grey x = rgb x x x
let grey_150 = grey 150
let grey_200 = grey 200

let lineto'' (x0,y0) (x,y) = moveto x0 y0 ; lineto x y
let lineto' o p = lineto'' (int_of_float_2 o) (int_of_float_2 p)
let curveto'' (x0,y0) p001 p011 p111 = moveto x0 y0 ; curveto p001 p011 p111
let curveto' p000 p001 p011 p111 = curveto'' (int_of_float_2 p000) (int_of_float_2 p001) (int_of_float_2 p011) (int_of_float_2 p111)

let rec coordonnees_absolues' orig (x0,y0) = function
  |[]                                 -> []
  |(Rmoveto,[x;y])                ::q -> (Moveto,[x0+.x;y0+.y]) :: coordonnees_absolues' (x0+.x,y0+.y)  (x0+.x,y0+.y) q
  |(Hmoveto,[x]  )                ::q -> coordonnees_absolues' orig  (x0,y0) ((Rmoveto,[x;0.])::q)
  |(Vmoveto,[y]  )                ::q -> coordonnees_absolues' orig  (x0,y0) ((Rmoveto,[0.;y])::q)
  |(Rlineto,[x;y])                ::q -> (Lineto,[x0;y0;x0+.x;y0+.y]) :: coordonnees_absolues' orig  (x0+.x,y0+.y) q
  |(Hlineto,[x]  )                ::q -> coordonnees_absolues' orig  (x0,y0) ((Rlineto,[x;0.])::q)
  |(Vlineto,[y]  )                ::q -> coordonnees_absolues' orig  (x0,y0) ((Rlineto,[0.;y])::q)
  |(Rrcurveto,[x1;y1;x2;y2;x3;y3])::q -> (Curveto,[x0;y0;x0+.x1;y0+.y1;x0+.x1+.x2;y0+.y1+.y2;x0+.x1+.x2+.x3;y0+.y1+.y2+.y3]) :: coordonnees_absolues' orig (x0+.x1+.x2+.x3,y0+.y1+.y2+.y3) q
  |(Vhcurveto,[y1;x2;y2;x3]      )::q -> coordonnees_absolues' orig  (x0,y0) ((Rrcurveto,[0.;y1;x2;y2;x3;0.])::q)
  |(Hvcurveto,[x1;x2;y2;y3]      )::q -> coordonnees_absolues' orig  (x0,y0) ((Rrcurveto,[x1;0.;x2;y2;0.;y3])::q)
  |_                              ::q -> let a,b = orig in
					 (Lineto,[x0;y0;a;b]) :: coordonnees_absolues' orig  (x0,y0) q

let coordonnees_absolues = coordonnees_absolues' (0.,0.) (0.,0.)

let rec trace_abs couleurs = function
  |[]                                     -> ()
  |(Moveto,[x;y])                     ::q ->
    moveto (int_of_float' x) (int_of_float' y) ;
    trace_abs couleurs q
  |(Lineto,[x0;y0;x1;y1])             ::q ->
    set_color (hd couleurs) ;
    lineto' (x0,y0) (x1,y1) ;
    trace_abs ((tl couleurs) @ [hd couleurs]) q
  |(Curveto,[x0;y0;x1;y1;x2;y2;x3;y3])::q ->
    set_color (hd couleurs) ;
    curveto' (x0,y0) (x1,y1) (x2,y2) (x3,y3) ;
    trace_abs ((tl couleurs) @ [hd couleurs]) q

let rec tracePoints = function
  |[] -> ()
  |(Moveto,[x;y])                     ::q ->
    tracePoints q
  |(Lineto,[x0;y0;x1;y1])             ::q ->
    fill_circle x0 y0 5 ;
    fill_circle x1 y1 5 ;
    tracePoints q
  |(Curveto,[x0;y0;x1;y1;x2;y2;x3;y3])::q ->
    fill_circle x0 y0 5 ;
    fill_circle x1 y1 5 ;
    fill_circle x2 y2 5 ;
    fill_circle x3 y3 5 ;
    tracePoints q
      
let rec translate' x y = function
  |[]      -> []
  |t::r::q -> (t+.x)::(r+.y)::translate' x y q

let translate x y = map (fun (i,a) -> i,translate' x y a)
  
let rec traceBoundingBox' couleurs = function
  |[]                           -> ()
  |((xmin,ymin),(xmax,ymax))::q -> set_color (hd couleurs) ;
    lineto'' (xmin,ymin) (xmin,ymax) ;
    lineto'' (xmin,ymax) (xmax,ymax) ;
    lineto'' (xmax,ymax) (xmax,ymin) ;
    lineto'' (xmax,ymin) (xmin,ymin) ;
    traceBoundingBox' ((tl couleurs) @ [hd couleurs]) q

let traceBoundingBox = traceBoundingBox' (rev [black;red;green;blue;yellow;brown;magenta;cyan])
(*
  #use "enregistre.ml";;
  #use "auxiliaires.ml";;
  #use "figures.ml";;
*)
