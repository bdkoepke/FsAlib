namespace Alib

module Chapter2 =
    let rec insertionSort xs = 
        let rec insert x = function
            | []      -> [x]
            | x'::xs' -> if x <= x' then x::x'::xs'
                         else x'::insert x xs'
        match xs with
        | []    -> []
        | x::xs -> insert x (insertionSort xs)

    let rec selectionSort = function
        | []    -> []
        | xs -> let min = List.min xs in
                let rest = List.filter (fun i -> i <> min) xs in
                min::(selectionSort rest)
    let rec findMatch xs ys =
        let rec findMatch' xs' ys ys' =
            match (xs', ys) with
            | ([], _) -> true
            | (_, []) -> false
            | (x::xs', y::ys) -> if x = y
                                    then findMatch' xs' ys ys'
                                    else findMatch' xs ys' (match ys' with
                                                            | []    -> []
                                                            | y::ys -> ys)
        match ys with
        | [] -> false
        | y::ys' -> findMatch' xs ys ys'
