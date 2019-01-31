type my_nonterminals =
  | Food | Fruit | Veggie | Meat | Hotpot

let my_grammar = 
  (Food, 
   function
       | Food ->
             [[N Hotpot; N Meat; N Veggie];
              [N Meat; N Fruit];
              [N Hotpot]]
       | Hotpot ->
             [[T"Haidilao"];
              [T"Lafu"];
              [T"Xiaolongkan"]]
       | Meat ->
             [[T"Lamb";T"Beef"];
              [T"Fish"]]
       | Veggie ->
             [[T"Lettuce"];
              [T"Mushroom"]]
       | Fruit ->
             [[]])

let my_acceptor derivation fragment = match fragment with
    | "Haidilao"::t -> Some (derivation,fragment)
    | "Xiaolongkan"::t -> Some (derivation,fragment)
    | _ -> None



(* test blank fragment *)
let test_1 = ((parse_prefix my_grammar my_acceptor []) = None)

(* test order of rules *)
(* test2: test Food -> [N Hotpot N Meat N Veggie] before Food -> [N Hotpot] *)
let test_2 =
    ((parse_prefix my_grammar my_acceptor ["Lafu" ; "Lamb";"Beef";"Mushroom";"Xiaolongkan"])= Some([Food,[N Hotpot; N Meat; N Veggie]; Hotpot, [T"Lafu"]; Meat, [T"Lamb";T"Beef"]; Veggie, [T"Mushroom"]],["Xiaolongkan"]))

(* test rules at the back of the list *)
let test_3 = 
    ((parse_prefix my_grammar my_acceptor ["Haidilao";"Xiaolongkan"]) = Some([Food,[N Hotpot]; Hotpot, [T"Haidilao"]],["Xiaolongkan"]))


(* test when fragment is not acceptable *)
let test_4 =
    ((parse_prefix my_grammar my_acceptor ["Lafu";"Fish"]) = None)

let test_5 =
    ((parse_prefix my_grammar my_acceptor ["Lamb";"Beef";"Haidilao"]) = Some([Food, [N Meat; N
 Fruit]; Meat, [T"Lamb";T"Beef"]; Fruit, []],["Haidilao"]))
