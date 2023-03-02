(*Question 1*)
let rec pow x n = 
  match n with
| 0 -> 1
| _ ->  x * pow x (n-1);;

let rec float_pow x n =
  match n with
  | 0 -> 1.0
  | _ -> x *. float_pow x (n-1);;

(*Question 2*)

let rec compress list = match list with
|[] -> []
|[x] -> [x]
|h :: t -> if h = (List.hd t) then compress t else h :: compress t;;

compress ["a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e"];;

(*Question3*)

let rec remove_if list pred = 
  match list with
| [] -> []
| h :: d ->
  if pred h then remove_if d pred
  else h :: remove_if d pred;;
  
remove_if [1;2;3;4;5] (fun x -> x mod 3 = 1);;

(*Question 4*)

let rec slice lst i j =
  match lst with
  | [] -> []
  | h :: d ->
      if i <= 0 && j <= 0 then []
      else if i <= 0 then h :: slice d 0 (j-1)
      else slice d (i-1) (j-1);;

slice ["a";"b";"c";"d";"e";"f";"g";"h"] 2 6;;

(*Question 5*)
let helperContain list h fl = match list with
| [] -> false
| [x] -> fl x h
| hd :: tl -> fl hd h ;;

let rec helperEquivs h list fl = match list with
| [] -> [[h]]
| [x] -> if helperContain x h fl then [x@[h]] else [x;[h]]
| hd::tl -> if helperContain hd h fl then [hd@[h]]@tl else [hd] @ (helperEquivs h tl fl);;
let equivs fl lst = let rec helper fl lst list = match lst with
|[] -> list
|[x] -> helperEquivs x list fl 
|h::d -> (helper fl d (helperEquivs h list fl))  in helper fl lst [];;

equivs (fun x y -> (=) (x mod 2) (y mod 2)) [1; 2; 3; 4; 5; 6; 7; 8;10];;
equivs (=) [1;2;3;4];;

(*Question 6*)

(* Returns true if n is prime, false otherwise *)
let is_prime n =
    if n <= 1 then false    (* 1 is not prime *)
    else if n <= 3 then true  (* 2 and 3 are prime *)
    else
      let rec is_not_divisible d =
        if d * d > n then true    (* n has no factors in [2..sqrt(n)] *)
        else if n mod d = 0 then false (* n is divisible by d *)
        else is_not_divisible (d + 1) (* try the next divisor *)
      in
      is_not_divisible 2;; (* start testing divisors from 2 *)

let goldbachpair x = let rec helper x y = match x with 
| 0 -> (0,0)
| h -> if is_prime h = true && is_prime y = true then (h,y) else helper (x-1) (y+1) 
in helper x 0;;

goldbachpair 1000;;

(*Question 7*)

let rec identical_on f g lst = match lst with 
| [x] -> (f x) = (g x)
| h :: d -> (f h) = (g h) && identical_on f g d
| [] -> false;;

let f i = i * i;;

let g i = 3 * i;;

identical_on f g [1;2;3];;

(*Question 8*)
let tempChecker temp = match temp with
| [] -> 1
| [x] -> 2
| _::_::_ -> 0;;


let tempResult num temp x f = match num with
| 1 -> x 
| 2 -> f (List.hd temp) x
| _ -> x;;

let rec pairwisefilter f list = let rec helper f temp list = match list with
| [] -> []
| [x] -> [tempResult (tempChecker temp) temp x f]
| h::d -> if tempChecker temp = 1 
  then helper f [h] d
  else tempResult 2 temp h f :: helper f [] d
in helper f [] list;;



pairwisefilter min [14; 11; 20; 25; 10; 11];;

(*Question 9*)
let get_tuple_x tuple = match tuple with
| (x,y) ->  x;;

let get_tuple_y tuple = match tuple with
| (x,y) ->  y;;

let rec power k num = match num with
| 0 -> 1
| h -> k * power k (num-1);;

let rec add_left list k= match list with
| [] -> 0
| [x] -> get_tuple_x x * power k (get_tuple_y x)
| h::d -> (get_tuple_x h) * power k (get_tuple_y h) + (add_left d k);;


let polynomial list n = add_left list n;;
let f n = polynomial [(3, 3); (-2, 1); (5, 0)] n;;

f 2;;

(*Question 10*)
let getInner list = match list with 
| [x] -> x 
| [] -> []
| _::_::_ -> [];;
let rec powersetHelper listD list= match list with
| [] -> []
| [x] -> [((getInner listD) @ [x])]
| h :: d -> [((getInner listD) @ [h])] @ (powersetHelper [((getInner listD) @ [h])] d) @ (powersetHelper listD d);;

let rec powerset list = match list with
| [] -> [[]]
| [x] -> [[x];[]]
| h :: d -> [[h]] @ (powersetHelper [[h]] d) @ powerset d;;

powerset [1; 2; 3;7;9];;