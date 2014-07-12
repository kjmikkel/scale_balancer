let rec print_list = function 
  [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l

let tuple_decode (n, elem) = 
  let rec add_one_elem i acc = 
      match i  with
      | 0 -> acc
      | i -> add_one_elem (i-1) (elem::acc)
  in add_one_elem n [] 

let rec equal_compare weight cert compare =
  match (weight, cert, compare) with
  |         _, (c1, c2), 0       -> c1 :: c2 :: []
  |  (w1, w2), (c1, c2), compare -> 
  let first_smallest = w1 < w2 in
  let smallest = if first_smallest then w1 else w2 in 
  let biggest  = if first_smallest then w2 else w1 in
  let biggest_divisible = biggest mod smallest = 0 in

  (* if the number we are trying to compare is less than the smallest value, we cannot solve the problem *)
  if compare < smallest then [] 
                        else
  
  (* if the number fully devisible with biggest, then we just add the number of times and we are done *)
  if compare mod biggest = 0 then 
    let big_number = compare / biggest in
    if first_smallest then c1 :: (c2 + big_number) :: []
                      else (c1 + big_number) :: c2 :: []

  (* if compare is fully divisible with the smallest value, and the biggest value is fully divisible with the smallest value, and we subtract the bigger value, do that and proceed to the recursive call *)
  else if compare mod smallest = 0 && biggest_divisible && compare >= biggest then if first_smallest then equal_compare weight (c1, c2 + 1) (compare - biggest)
                                                                                                     else equal_compare weight (c1 + 1, c2) (compare - biggest) 
       
       (* otherwise subtract the smallest number and continues to the next iteration *)
       else if first_smallest then equal_compare weight (c1 + 1, c2) (compare - smallest)
                              else equal_compare weight (c1, c2 + 1) (compare - smallest);;


let scale_balancer_aux weight_1 weight_2 cert_1 cert_2 =
  match (weight_1, weight_2, cert_1, cert_2) with
  | ((w1, w2), (w3, w4), (0, 0), (0, 0)) -> 1 :: 2 :: 3 :: 4 :: []
  | ((w1, w2), (w3, w4), (c1, c2), (c3, c4)) -> 
  if (w1 * c1 + w2 * c2) = (w3 * c3 + w4 * c4) then c1 :: c2 :: c3 :: c4 :: [] 
                                               else 2 :: 3 :: 4 :: 5 :: [] ;;

let scale_balancer weight_1 weight_2 =
  scale_balancer_aux weight_1 weight_2 (0, 0) (0, 0);;

(* print_list (scale_balancer (1, 3) (2,4)) ;; *)

print_list (equal_compare (7, 11) (0, 0) 22) ;;