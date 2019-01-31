type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let convert_grammar gram1 = 
    let rec match_expr expr = function
        | [] -> []
        | (ex,elist)::t when expr = ex -> elist::(match_expr expr t)
        | (ex,elist)::t -> match_expr expr t

    in ((fst gram1), fun expr -> match_expr expr (snd gram1))





(* see if one rule can be written in terminals *)
let rec find_prefix rule prod_func accept d frag  = match rule with
    |[] -> accept d frag
    |(N nh)::t -> match_next prod_func nh (find_prefix t prod_func accept) d frag (prod_func nh)
    |(T th)::t -> match frag with (* to check the fragment *)
              |[] -> None
              |fh::ft when fh=th -> find_prefix t prod_func accept d ft
              |fh::ft -> None


(* match one by one. take a list.  If there is no expression in terminals, try next rule *)
and match_next prod_func sym_name accept d frag = function (* prod_func start expr *)
    | [] -> None
    | h::others -> match (find_prefix h prod_func accept (d@[sym_name, h]) frag) with 
                       |None -> match_next prod_func sym_name  accept d frag others
                       |result -> result 


let parse_prefix gram = (fun accept frag -> match_next (snd gram) (fst gram) accept [] frag ((snd gram) (fst gram)) )
