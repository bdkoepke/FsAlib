namespace Alib

module Chapter3 =
    type List'<'a when 'a : equality> =
        | Cons of 'a * List'<'a>
        | Nil with
        member this.search x = function
            | Nil -> None
            | Cons (a, xs) ->
                if x = a then Some a
                else this.search x xs
        member this.insert x = Cons (x, this)
        member this.delete x =
            let rec delete' xs x =
                match xs with
                | Nil -> Nil
                | Cons (a, xs) ->
                    if a = x then xs
                    else Cons (a, delete' xs x)
            delete' this x
    let rec fromList xs =
        match xs with
        | []    -> Nil
        | x::xs -> Cons (x, fromList xs)
    let rec toList xs =
        match xs with
        | Nil          -> []
        | Cons (x, xs) -> x::toList xs