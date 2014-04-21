module LoadMarketData

open System.IO

/// Loads market data for curve stripping
let loadCurveInputs csvFilePath = 
    System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__  // point the script here ...
    let lines = File.ReadAllLines(csvFilePath)
    let m     = (lines.[0]).Split(',') |> Seq.where (fun str -> not (str = "")) |> Seq.map (fun s -> float s)     |>  Array.ofSeq
    let U     =  lines.[1] .Split(',') |> Seq.where (fun str -> not (str = "")) |> Seq.map (fun str -> float str) |> Array.ofSeq
    let C = 
        lines 
        |> Seq.skip 2  // skip market price vector and cash flow dates
        |> Seq.map (fun line -> line.Split(',') |> Seq.where (fun str -> not (str = "")) |> Seq.map (fun s -> float s) |> List.ofSeq)
        |> List.ofSeq   
    m, U, C