let rectanglesGauche f a b n =
  let h = (b -. a) /. float_of_int n in
  let res = ref 0. in
  for i = 0 to n-1 do
    res := !res +. f (a +. float_of_int i *. h);
  done;
  h *. !res
;;

let simpson f a b n =
  let h = (b -. a) /. float_of_int n in
  let res = ref 0. in
  for i = 1 to n-1 do
    res := !res +. f (a +. float_of_int i *. h) +. 2. *. f (a +. (float_of_int i +. 0.5) *. h)
  done;
  res := !res +. 2. *. (f a +. f b);
  h *. !res /. 3.
;;

let derive f h t = (f (t +. h) -. f (t -. h)) /. 2. /. h ;;

let aireCubique (x0,y0) (x1,y1) (x2,y2) (x3,y3) =
  let n = 10 in
  let h = 1. /. float_of_int n in

  let x t =
    let u = 1. -. t in
    pow_fl u 3 *. float_of_int x0 +. 3. *. t *. pow_fl u 2 *. float_of_int x1 +. 3. *. pow_fl t 2 *. u *. float_of_int x2 +. pow_fl t 3 *. float_of_int x3
  in

  let y t =
    let u = 1. -. t in
    pow_fl u 3 *. float_of_int y0 +. 3. *. t *. pow_fl u 2 *. float_of_int y1 +. 3. *. pow_fl t 2 *. u *. float_of_int y2 +. pow_fl t 3 *. float_of_int y3
  in
  
  simpson (fun t -> (x t *. (derive y h t)) -. y t *. (derive x h t)) 0. 1. n /. 2.
;;
