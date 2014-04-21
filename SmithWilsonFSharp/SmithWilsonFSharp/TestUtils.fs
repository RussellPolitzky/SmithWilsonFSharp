module TestUtils

open FsUnit
open NUnit.Framework

/// Utilityt method to compare tuples.
let compareTuple (tpl1:(float * float)) (tpl2:(float * float)) =
    fst tpl1 |> should (equalWithin 1e-12) (fst tpl2)
    snd tpl1 |> should (equalWithin 1e-12) (snd tpl2)


/// Utility method to compare two 2D arrays.
let arrayShouldEqual (precision:float) (a1:float[,]) (a2:float[,])  =
    let a1d1 = a1.GetLength(0)
    let a2d1 = a2.GetLength(0)
    let a1d2 = a1.GetLength(1)
    let a2d2 = a2.GetLength(1)
    a1d1 |> should equal a2d1 
    a1d2 |> should equal a2d2
    for i in 0..(a1d1-1) do
        for j in 0..(a1d2-1) do
            a1.[i,j] |> should (equalWithin precision) a2.[i,j]