let rec print_list = function 
  [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l

let calc_total_weight weight cert =
  match (weight, cert) with (w1, w2), (c1, c2) -> c1 * w1 + c2 * w2

let tuple_to_list (a, b)  = a :: b :: [];;

let print_certificates (cert_1, cert_2) weight_1 weight_2 =
  match (cert_2, weight_1, weight_2) with (c3, c4), (w1, w2), (w3, w4) ->
    print_string "Certificate 1:\n" ; 
    print_list cert_1 ; 
    print_string "\nWeight:\n" ; print_list (w1 :: w2 :: []) ;
    print_string "\n\nCertificate 2:\n" ; print_list (c3 :: c4 :: []) ; 
    print_string "\nWeight:\n" ; print_list (w3 :: w4 :: []) ; 
    print_string "\n\nTotal weight: " ; print_int (calc_total_weight weight_2 cert_2) ; 
    print_string "\n" ;; 
   



(* Given a compare weight and two types of weight, the system figures out how many of each are need (if it can be solved) to weigh the same as the compare value  *)
let rec equal_compare weight cert compare =
  match (weight, cert, compare) with
  |        _, (c1, c2), 0       -> c1 :: c2 :: []
  | (w1, w2), (c1, c2), compare -> 
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


let increase_capacity weight cert =
  match (weight, cert) with
  | ((w1, w2), (c1, c2)) -> 
  
  let first_smallest = w1 < w2 in
  let biggest = if first_smallest then w2 else w1 in
  
  (* We find out if the increase in the smallest will go over the biggest number *)
  let next_smallest_total = if first_smallest then w1 * (c1 + 1) else c2 * (w2 + 1) in
  let bigger_than_biggest = biggest < next_smallest_total in
  
  (* We figure out the new certificate  *)
  let new_cert = if bigger_than_biggest then if first_smallest then (0, c2 + 1)
                                                               else (c1 + 1, 0)
                                        else if first_smallest then (c1 + 1, c2)
                                                               else (c1, c2 + 1) in
  (* return the new certificate *)
  new_cert ;;

let scale_balance weight_1 weight_2 =
  let rec scale_balance_aux weight_1 weight_2 cert =
    let new_cert = increase_capacity weight_2 cert in
    let weight_1_cert = equal_compare weight_1 (0, 0) (calc_total_weight weight_2 new_cert) in
    if weight_1_cert == [] then scale_balance_aux weight_1 weight_2 new_cert
                    else (weight_1_cert, new_cert)  
  in 
  scale_balance_aux weight_1 weight_2 (0, 0);;

let weight_1 = (21, 105) in
let weight_2 = (13, 9) in
let result = scale_balance weight_1 weight_2 in
print_certificates result weight_1 weight_2 ;;