#use "/home/clementguidi/.opam/system/lib/toplevel/topfind";;
#require "graphicspdf";;
open Graphicspdf;;

let enregistre f nom_fichier (x,y) =
  open_pdf ("/home/clementguidi/Documents/Cours/Prépa/MP/TIPE/Images/" ^ nom_fichier ^ ".pdf") ;
  open_graph (string_of_int x ^ "x" ^ string_of_int y) ;
  f () ;
  close_graph () ;
  clear_graph ()
