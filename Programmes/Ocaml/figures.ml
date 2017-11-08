let f1 () =
  let n = 15 in
  set_text_size n ;
  let p000,p001,p011,p111 = (40.,30.),(90.,210.),(240.,300.),(300.,60.) in
  set_color grey_150 ;
  lineto' p000 p001 ;
  lineto' p001 p011 ;
  lineto' p011 p111 ;
  set_color black ;
  curveto' p000 p001 p011 p111 ;
  affiche_point n So p000 "P000" ;
  affiche_point n No p001 "P001" ;
  affiche_point n Ne p011 "P011" ;
  affiche_point n Se p111 "P111"
    
let f2 () =
  let n = 15 in
  set_text_size n ;
  let t = 1./.2. in
  let u = 1. -. t in
  let p000,p001,p011,p111 = (40.,30.),(90.,210.),(240.,300.),(300.,60.) in
  let p00t,p0t1,pt11 = bar2 [(p000,t);(p001,u)],bar2 [(p001,t);(p011,u)],bar2 [(p011,t);(p111,u)] in
  set_color grey_150 ;
  lineto' p000 p001 ;
  lineto' p001 p011 ;
  lineto' p011 p111 ;
  lineto' p00t p0t1 ;
  lineto' p0t1 pt11 ;
  set_color black ;
  curveto' p000 p001 p011 p111 ;
  affiche_point n So p000 "P000" ;
  affiche_point n No p001 "P001" ;
  affiche_point n Ne p011 "P011" ;
  affiche_point n Se p111 "P111" ;
  affiche_point n No p00t "P00t" ;
  affiche_point n No p0t1 "P0t1" ;
  affiche_point n Ne pt11 "Pt11"
    
let f3 () =
  let n = 15 in
  set_text_size n ;
  let t = 1./.2. in
  let u = 1. -. t in
  let p000,p001,p011,p111 = (40.,30.),(90.,210.),(240.,300.),(300.,60.) in
  let p00t,p0t1,pt11 = bar2 [(p000,t);(p001,u)],bar2 [(p001,t);(p011,u)],bar2 [(p011,t);(p111,u)] in
  let p0tt,ptt1 = bar2 [(p00t,t);(p0t1,u)],bar2 [(p0t1,t);(pt11,u)] in
  set_color grey_150 ;
  lineto' p000 p001 ;
  lineto' p001 p011 ;
  lineto' p011 p111 ;
  lineto' p00t p0t1 ;
  lineto' p0t1 pt11 ;
  lineto' p0tt ptt1 ;
  set_color black ;
  curveto' p000 p001 p011 p111 ;
  affiche_point n So p000 "P000" ;
  affiche_point n No p001 "P001" ;
  affiche_point n Ne p011 "P011" ;
  affiche_point n Se p111 "P111" ;
  affiche_point n No p00t "P00t" ;
  affiche_point n No p0t1 "P0t1" ;
  affiche_point n Ne pt11 "Pt11" ;
  affiche_point n No p0tt "P0tt" ;
  affiche_point n Ne ptt1 "Ptt1"

let f4 () =
  let n = 15 in
  set_text_size n ;
  let t = 1./.2. in
  let u = 1. -. t in
  let p000,p001,p011,p111 = (40.,30.),(90.,210.),(240.,300.),(300.,60.) in
  let p00t,p0t1,pt11 = bar2 [(p000,t);(p001,u)],bar2 [(p001,t);(p011,u)],bar2 [(p011,t);(p111,u)] in
  let p0tt,ptt1 = bar2 [(p00t,t);(p0t1,u)],bar2 [(p0t1,t);(pt11,u)] in
  let pttt = bar2 [(p0tt,t);(ptt1,u)] in
  set_color grey_150 ;
  lineto' p000 p001 ;
  lineto' p001 p011 ;
  lineto' p011 p111 ;
  lineto' p00t p0t1 ;
  lineto' p0t1 pt11 ;
  lineto' p0tt ptt1 ;
  set_color black ;
  curveto' p000 p001 p011 p111 ;
  affiche_point n So p000 "P000" ;
  affiche_point n No p001 "P001" ;
  affiche_point n Ne p011 "P011" ;
  affiche_point n Se p111 "P111" ;
  affiche_point n No p00t "P00t" ;
  affiche_point n No p0t1 "P0t1" ;
  affiche_point n Ne pt11 "Pt11" ;
  affiche_point n No p0tt "P0tt" ;
  affiche_point n Ne ptt1 "Ptt1" ;
  affiche_point n Se pttt "Pttt"
    
let g4 () =
  let n = 10 in
  set_text_size n ;
  let t = 1./.3. in
  let u = 1. -. t in
  let p000,p001,p011,p111 = (110.,30.),(320.,320.),(50.,320.),(220.,30.) in
  let p00t,p0t1,pt11 = bar2 [(p000,t);(p001,u)],bar2 [(p001,t);(p011,u)],bar2 [(p011,t);(p111,u)] in
  let p0tt,ptt1 = bar2 [(p00t,t);(p0t1,u)],bar2 [(p0t1,t);(pt11,u)] in
  let pttt = bar2 [(p0tt,t);(ptt1,u)] in
  set_color grey_150 ;
  lineto' p000 p001 ;
  lineto' p001 p011 ;
  lineto' p011 p111 ;
  lineto' p00t p0t1 ;
  lineto' p0t1 pt11 ;
  lineto' p0tt ptt1 ;
  set_color black ;
  curveto' p000 p001 p011 p111 ;
  affiche_point n So p000 "P000" ;
  affiche_point n Ne p001 "P001" ;
  affiche_point n No p011 "P011" ;
  affiche_point n Se p111 "P111" ;
  affiche_point n Se p00t "P00t" ;
  affiche_point n No p0t1 "P0t1" ;
  affiche_point n So pt11 "Pt11" ;
  affiche_point n Ne p0tt "P0tt" ;
  affiche_point n No ptt1 "Ptt1" ;
  affiche_point n Se pttt "Pttt"

let b1 () =
  let b =
    [Hmoveto, [167.0];
     Vlineto, [133.0];
     Rrcurveto, [71.0; -105.0; 99.0; -52.0; 125.0; 0.0];
     Rrcurveto, [126.0; 0.0; 108.0; 50.0; 90.0; 99.0];
     Rrcurveto, [90.0; 99.0; 45.0; 141.0; 0.0; 182.0];
     Rrcurveto, [0.0; 77.0; -11.0; 72.0; -21.0; 67.0];
     Rrcurveto, [-21.0; 67.0; -30.0; 58.0; -38.0; 48.0];
     Rrcurveto, [-37.0; 47.0; -48.0; 37.0; -59.0; 26.0];
     Rrcurveto, [-58.0; 27.0; -62.0; 13.0; -65.0; 0.0];
     Rrcurveto, [-118.0; 0.0; -97.0; -48.0; -76.0; -95.0];
     Vlineto, [523.0];
     Hlineto, [-180.0];
     Vlineto, [-1466.0];
     Closepath, [];
     Rmoveto, [165.0; 539.0];
     Rrcurveto, [0.0; 129.0; 29.0; 99.0; 57.0; 69.0];
     Rrcurveto, [57.0; 68.0; 68.0; 34.0; 79.0; 0.0];
     Rrcurveto, [78.0; 0.0; 67.0; -33.0; 54.0; -66.0];
     Rrcurveto, [56.0; -66.0; 27.0; -102.0; 0.0; -139.0];
     Rrcurveto, [0.0; -135.0; -29.0; -102.0; -57.0; -69.0];
     Rrcurveto, [-57.0; -68.0; -68.0; -34.0; -79.0; 0.0];
     Rrcurveto, [-97.0; 0.0; -76.0; 46.0; -57.0; 93.0];
     Rrcurveto, [-35.0; 57.0; -17.0; 92.0; 0.0; 127.0];
     Closepath, [];
     Endchar, []] in
  let (xmin,ymin),(xmax,ymax) = (0.0, -24.0), (921.0, 1466.0) in
  let b = (Rmoveto,[-.xmin;-.ymin])::b in
  let b = coordonnees_absolues b in
  trace_abs [black;red;green;blue;yellow;brown;magenta;cyan] b

let b2 () =
  let b =
    [Hmoveto, [167.0];
     Vlineto, [133.0];
     Rrcurveto, [71.0; -105.0; 99.0; -52.0; 125.0; 0.0];
     Rrcurveto, [126.0; 0.0; 108.0; 50.0; 90.0; 99.0];
     Rrcurveto, [90.0; 99.0; 45.0; 141.0; 0.0; 182.0];
     Rrcurveto, [0.0; 77.0; -11.0; 72.0; -21.0; 67.0];
     Rrcurveto, [-21.0; 67.0; -30.0; 58.0; -38.0; 48.0];
     Rrcurveto, [-37.0; 47.0; -48.0; 37.0; -59.0; 26.0];
     Rrcurveto, [-58.0; 27.0; -62.0; 13.0; -65.0; 0.0];
     Rrcurveto, [-118.0; 0.0; -97.0; -48.0; -76.0; -95.0];
     Vlineto, [523.0];
     Hlineto, [-180.0];
     Vlineto, [-1466.0];
     Closepath, [];
     Rmoveto, [165.0; 539.0];
     Rrcurveto, [0.0; 129.0; 29.0; 99.0; 57.0; 69.0];
     Rrcurveto, [57.0; 68.0; 68.0; 34.0; 79.0; 0.0];
     Rrcurveto, [78.0; 0.0; 67.0; -33.0; 54.0; -66.0];
     Rrcurveto, [56.0; -66.0; 27.0; -102.0; 0.0; -139.0];
     Rrcurveto, [0.0; -135.0; -29.0; -102.0; -57.0; -69.0];
     Rrcurveto, [-57.0; -68.0; -68.0; -34.0; -79.0; 0.0];
     Rrcurveto, [-97.0; 0.0; -76.0; 46.0; -57.0; 93.0];
     Rrcurveto, [-35.0; 57.0; -17.0; 92.0; 0.0; 127.0];
     Closepath, [];
     Endchar, []] in
  let (xmin,ymin),(xmax,ymax) = (0.0, -24.0), (921.0, 1466.0) in
  let b = (Rmoveto,[-.xmin+.5.;-.ymin+.5.])::b in
  let b = coordonnees_absolues b in
  trace_abs [black;red;green;blue;yellow;brown;magenta;cyan] b ;
  set_color black ;
  tracePoints (map (fun (i,a) -> i,map int_of_float' a) b)
    
let b3 () =
  let b =
    [Hmoveto, [167.0];
     Vlineto, [133.0];
     Rrcurveto, [71.0; -105.0; 99.0; -52.0; 125.0; 0.0];
     Rrcurveto, [126.0; 0.0; 108.0; 50.0; 90.0; 99.0];
     Rrcurveto, [90.0; 99.0; 45.0; 141.0; 0.0; 182.0];
     Rrcurveto, [0.0; 77.0; -11.0; 72.0; -21.0; 67.0];
     Rrcurveto, [-21.0; 67.0; -30.0; 58.0; -38.0; 48.0];
     Rrcurveto, [-37.0; 47.0; -48.0; 37.0; -59.0; 26.0];
     Rrcurveto, [-58.0; 27.0; -62.0; 13.0; -65.0; 0.0];
     Rrcurveto, [-118.0; 0.0; -97.0; -48.0; -76.0; -95.0];
     Vlineto, [523.0];
     Hlineto, [-180.0];
     Vlineto, [-1466.0];
     Closepath, [];
     Rmoveto, [165.0; 539.0];
     Rrcurveto, [0.0; 129.0; 29.0; 99.0; 57.0; 69.0];
     Rrcurveto, [57.0; 68.0; 68.0; 34.0; 79.0; 0.0];
     Rrcurveto, [78.0; 0.0; 67.0; -33.0; 54.0; -66.0];
     Rrcurveto, [56.0; -66.0; 27.0; -102.0; 0.0; -139.0];
     Rrcurveto, [0.0; -135.0; -29.0; -102.0; -57.0; -69.0];
     Rrcurveto, [-57.0; -68.0; -68.0; -34.0; -79.0; 0.0];
     Rrcurveto, [-97.0; 0.0; -76.0; 46.0; -57.0; 93.0];
     Rrcurveto, [-35.0; 57.0; -17.0; 92.0; 0.0; 127.0];
     Closepath, [];
     Endchar, []] in
  let (xmin,ymin),(xmax,ymax) = (0.0, -24.0), (921.0, 1466.0) in
  let b = (Rmoveto,[-.xmin;-.ymin])::b in
  let b = coordonnees_absolues b in
  trace_abs [black;red;green;blue;yellow;brown;magenta;cyan] b ;
  let bbb =
    [(0, 0), (921, 1490);
     (0, 0), (921, 1490);
     (0, 0), (921, 1490);
     (0, 0), (921, 1490);
     (0, 0), (921, 1490);
     (0, 0), (921, 1490);
     (0, 0), (921, 1490);
     (0, 0), (921, 1490);
     (0, 0), (921, 1490);
     (0, 0), (921, 1490);
     (0, 0), (921, 1490);
     (0, 0), (921, 1490);
     (0, 0), (921, 1490);
     (0, 0), (921, 1490);
     (167, 0), (921, 1490);
     (167, 0), (921, 1110);
     (167, 0), (921, 1110);
     (167, 0), (921, 1070);
     (167, 0), (921, 960);
     (167, 0), (921, 787);
     (167, 0), (921, 571);
     (167, 0), (786, 157);
     (167, 0), (462, 157);
     (167, 24), (167, 157);
     (167, 24), (167, 24)] in
  traceBoundingBox bbb
  
  
(*
  #use "enregistre.ml";;
  #use "auxiliaires.ml";;
  #use "figures.ml";;
  enregistre f1 "f1" (350,350);;
  enregistre f2 "f2" (350,350);;
  enregistre f3 "f3" (350,350);;
  enregistre f4 "f4" (350,350);;
  enregistre g4 "g4" (350,350);;
  enregistre b1 "b1" (921,1490);;
  enregistre b2 "b2" (931,1500);;
  enregistre b3 "b3" (921,1490);;
*)
