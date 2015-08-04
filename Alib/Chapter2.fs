namespace Alib

open Microsoft.FSharp.Math

module Chapter2 =
    (* non in-place insertion sort *)
    let rec insertionSort xs =
        let rec insert x = function
        | []     -> [x]
        | x'::xs -> if x <= x' then x::x'::xs
                    else x'::insert x xs
        match xs with
        | []    -> []
        | x::xs -> insert x (insertionSort xs)
    (* non in-place selection sort *)
    let rec selectionSort = function
        | [] -> []
        | xs -> let x = List.min xs
                let xs' = xs |> List.filter (fun x' -> x' <> x)
                x::(selectionSort xs')
    (* finds a match for the specified prefix in
       the specified text *)
    let rec findMatch xs ys =
        let safeHead = function
        | []    -> []
        | y::ys -> ys
        let rec findMatch' xs' ys ys' =
            match (xs', ys) with
            | ([], _)         -> true
            | (_, [])         -> false
            | (x::xs', y::ys) -> if x = y
                                 then findMatch' xs' ys  ys'
                                 else findMatch' xs  ys' (safeHead ys')
        match ys with
        | []     -> false
        | y::ys' -> findMatch' xs ys ys'
    (* fast exponentiation *)
    let power x y =
        if y = 0 then Some 1
        elif y < 0 then None
        else let rec power' x y =
                let isEven x = (x % 2) = 0 in
                if y = 1 then x
                elif isEven y then power' (x * x) (y / 2)
                else x * power' x (y - 1)
             Some (power' x y)
    (* mutable matrix multiplication *)
    let matrixMultiplication (A: matrix) (B: matrix) =
        let (x, y) = A.Dimensions
        let (y, z) = B.Dimensions
        Matrix.init x z (fun i j -> 
            [ 1 .. z ] |>
            List.fold (fun x k ->
                       x + (Matrix.get A i k) + (Matrix.get B k j)) 0.0)