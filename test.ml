let rec last a = 
    match a with
    | [] -> None
    | h1::h2::t -> last (h2::t)
    | h::t -> Some h

let rec last_two a = 
    match a with
    | [] -> None
    | h1::h2::h3::t -> last_two (h2::h3::t)
    | h1::h2::t -> Some (h1,h2)
    | h::t -> None

let rec at lst index =
    match lst with
    | [] -> None
    | h::t -> (if index > 0 then at t (index - 1) else Some h)

let length lst =
    let rec calculate_length myLst index =
        match myLst with
        | [] -> index
        | h::t -> calculate_length t (index + 1)
    in calculate_length lst 0
    
let rev lst = 
    let rec create_list oldlst newlst =
        match oldlst with
        | [] -> newlst
        | h::t -> create_list t (h::newlst)
    in create_list lst []

let is_palindrome lst =
    let reversed = rev lst in
    if lst = reversed then true else false

let compress lst =
    let rec get_compressed oldLst newLst prevChr = 
        match oldLst with
        | [] -> newLst
        | h::t -> if h = prevChr then (get_compressed t newLst prevChr) else (get_compressed t (h::newLst) h)
    in rev (get_compressed lst [] '\n')

let duplicate lst =
    let rec create_duplicates oldLst newLst =
        match oldLst with
        | [] -> newLst
        | h::t -> create_duplicates t (h::h::newLst)
    in rev (create_duplicates lst [])

let replicate lst index = 
    let rec get_addition element index =
        if index = 0 then [] else element::(get_addition element (index - 1))
    in
    let rec create_replicates oldLst newLst index =
        match oldLst with
        | [] -> newLst
        | h::t -> create_replicates t ((get_addition h index)@newLst) index
    in rev (create_replicates lst [] index)

let drop lst index =
    let rec perform_drop lst curr_index starting_index =
        match lst with
        | [] -> []
        | h::t -> if (curr_index - 1) mod starting_index = 0 then perform_drop t (curr_index - 1) starting_index else h::(perform_drop t (curr_index - 1)) starting_index
    in perform_drop lst index index

let split lst index =
    let rec get_new_list oldlst newlst num =
        match oldlst with
        | [] -> (newlst, [])
        | h::t -> if num = 0 then (newlst@[h], t) else get_new_list t (newlst@[h]) (num - 1)
    in get_new_list lst [] (index - 1)

let slice lst i k =
    let rec get_lst newlst num1 num2 curr = 
        match newlst with
        | [] -> []
        | h::t -> if curr >= num1 && curr <= num2 then h::(get_lst t num1 num2 (curr + 1)) else get_lst t num1 num2 (curr + 1)
    in get_lst lst i k 0

let rec rotate lst i =
    if i < 0 then rotate lst (i + (length lst)) else
    match lst with
    | [] -> []
    | h::t -> if i = 0 then lst else rotate (t@[h]) (i - 1)

let rec remove_at index lst =
    match lst with
    | [] -> []
    | h::t -> if index = 0 then remove_at (index - 1) t else h::(remove_at (index - 1) t)

let rec insert_at element i lst =
    if i = 0 then element::lst else
    match lst with
    | [] -> if i > 0 then [element] else []
    | h::t -> h::(insert_at element (i - 1) t)

let rec range num1 num2 = 
    if num1 > num2 then num1::(range (num1 - 1) num2) else if num2 > num1 then num1::(range (num1 + 1) num2) else [num1]

let is_prime i =
    let rec determine_prime final curr = 
        if final = curr then true else if final mod curr = 0 then false else determine_prime final (curr + 1)
    in if i = 1 then false else determine_prime i 2

let rec gcd a b =
    if b = 0 then a else gcd b (a mod b)

let rec coprime a b = gcd a b = 1

let phi i = 
    let rec get_num start finish =
        if start = finish then 0 else if coprime start finish then 1 + get_num (start + 1) finish else get_num (start + 1) finish
    in get_num 1 i

let factors i =
    let rec get_list start num =
        if start <= num then (if is_prime start && num mod start = 0 then start::(get_list 1 (num / start)) else get_list (start + 1) num) else []
    in get_list 1 i