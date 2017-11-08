type commande =
  | Rmoveto
  | Vmoveto
  | Hmoveto
  | Rlineto
  | Vlineto
  | Hlineto
  | Rrcurveto
  | Vhcurveto
  | Hvcurveto
  |Closepath
  | Endchar
;;
type arguments == float list ;;
type instruction == commande * arguments ;;
type definition == instruction list ;;
type glyphe == char * definition ;;
type fonte == glyphe list ;;
type point == float * float ;;
type boundingBox == point * point ;;
