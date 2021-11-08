type 'a node = {
    left_s: 'a queue;
    right_s: 'a queue;
    value: 'a;
    depth: int} 
and 'a queue = 
    | Leaf
    | Node of ('a node);;

exception Empty;;

let swich q1 q2 = match q1 with
    | Leaf -> q2,q1
    | Node({left_s=_;right_s=_;value=x;depth=_}) -> match q2 with 
        | Leaf ->q1,q2
        | Node({left_s=_;right_s=_;value=y;depth=_}) -> if x<=y then q1,q2 else q2,q1;;

let read_depth q = match q with
    | Leaf -> 0
    | Node({left_s=_;right_s=_;value=_;depth=d}) -> d;;

let rec join q1 q2 = 
    let q1,q2 = swich q1 q2 in match q1 with
        | Leaf -> q1
        | Node({left_s=l;right_s=r;value=v;depth=d}) -> let r = join r q2 in
            if read_depth l >= read_depth r 
                then Node({left_s=l;right_s=r;value=v;depth=(read_depth r)+1})
                else Node({left_s=r;right_s=l;value=v;depth=(read_depth l)+1});;  

let empty = Leaf;;

let add x q = 
    join (Node({left_s=Leaf;right_s=Leaf;value=x;depth=1})) q;;

let delete_min q = match q with 
    | Leaf -> raise Empty
    | Node({left_s=l;right_s=r;value=v;depth=d}) -> (v,join l r);;

let is_empty q = match q with 
    | Leaf -> true
    | _ -> false;;


(*tests *)
let q = add 4 (add 3 (add 2 (add 1 empty)));;
