namespace AlibTest.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open NHamcrest.Core
open Alib.Chapter2

type ``Sort tests`` (sort)=
   let sorted = [1; 2; 3; 4; 5; 6; 7; 8; 9]
   let unsorted = [3; 7; 4; 9; 8; 5; 2; 6; 1]
   let isSorted = List.forall (fun (a, b) -> a = b)
   let shouldBeSorted a b =
       List.zip a b |> isSorted |> should equal true
   [<TestMethod>] member test.
    ``Test sort for ten elements.`` ()=
          shouldBeSorted sorted (sort unsorted)
   [<TestMethod>] member test.
    ``Test empty sort.`` ()=
          shouldBeSorted [] (sort [])

[<TestClass>]
type ``Insertion sort tests`` ()=
    inherit ``Sort tests``(insertionSort)

[<TestClass>]
type ``Selection sort tests`` ()=
    inherit ``Sort tests``(selectionSort)

module String =
    let explode (s:string) =
        [for c in s -> c]
    let implode (xs:char list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()

[<TestClass>]
type ``Find match tests`` ()=
    [<TestMethod>] member test.
     ``Test simple pattern.`` ()=
            findMatch  (String.explode "pattern")
                       (String.explode "this is a pattern") |>
                should equal true
[<TestClass>]
type ``Test power`` ()=
    [<TestMethod>] member test.
     ``Test power 2^16.`` ()=
            should equal (Some 65536) (power 2 16)
    [<TestMethod>] member test.
     ``Test power 3^5.`` ()=
            should equal (Some 243) (power 3 5)
    [<TestMethod>] member test.
     ``Test power 243^1.`` ()=
            should equal (Some 243) (power 243 1)
    [<TestMethod>] member test.
     ``Test power 2^0.`` ()=
            should equal (Some 1) (power 2 0)
    [<TestMethod>] member test.
     ``Test power 2^(-1).`` ()=
            should equal None (power 2 (-1))