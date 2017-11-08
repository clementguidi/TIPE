#open "graphics";;
open_graph "1600x900";;

include "Types"              ;;
include "Utilitaires"        ;;
include "RecuperationAsm"    ;;
include "BoundingBox"        ;;
include "TracéCourbes"       ;;
include "FréquenceApparition";;
include "CalculAire"         ;;

let Arial           = recupereFonte "Arial"               ;;
let Times_New_Roman = recupereFonte "Times_New_Roman"     ;;
let Comic_Sans_MS   = recupereFonte "Comic_Sans_MS"       ;;
let Courier_New     = recupereFonte "Courier_New"         ;;
let DejaVuSans      = recupereFonte "DejaVuSans"          ;;
let Garamond        = recupereFonte "EBGaramond12-Regular";;
let Nimbus          = recupereFonte "Nimbus_Roman_No9_L"  ;;

let aires = map airesPonderees [Arial;Times_New_Roman;Comic_Sans_MS;Courier_New;DejaVuSans;Garamond];;
