module SmithWilsonFSharp.Tests

open FsUnit
open NUnit.Framework
open System
open SmithWilson
open Rates
open System.IO
open ArrayExtensions
open LoadMarketData
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Double.Vector


[<Test>]  
let ``should be able to match EIOPA sample 1 rate``()=
    let α = 0.1                    // Mean reversion parameter controls rate at which curve reverts to UFR.
    let UFR = Math.Log(1.0 + 0.042)  // Ultimate Forward Rate expressed in NACC (converted from 0.042 NACA)
    let m = [| 1.0; 1.0; 1.0; 1.0 |] // market prices
    let cashFlowDates = [| 1.0; 2.0; 3.0; 4.0; 5.0 |] 
    let C = // Cash flow matrix
        [
           [  1.01;  0.00;  0.00;  0.00;  0.00 ] // instrument 1
           [  0.02;  1.02;   0.0;   0.0;   0.0 ] // instrument 2
           [ 0.026; 0.026; 1.026;   0.0;   0.0 ] // instrument 3
           [ 0.034; 0.034; 0.034; 0.034; 1.034 ] // instrument 4
        ]
    let t        = 4.0
    let result   = PtSmithWilson α UFR m C cashFlowDates [| t |]
    let df4      = result.[0,1]
    let nacaRate = dfToNaca t df4
    nacaRate |> should (equalWithin 0.0001) 0.031


[<Test>]  
let ``should be able to match EIOPA sample 2 rate``()=
    let α = 0.1                      // Mean reversion parameter controls rate at which curve reverts to UFR.
    let UFR = Math.Log(1.0 + 0.042)  // Ultimate Forward Rate expressed in NACC (converted from 0.042 NACA)
    let m = [| 1.0; 1.0; 1.0; 1.0 |] // market prices
    let cashFlowDates = [| 0.25 .. 0.25 .. 5.0 |]  
    let C = // Cash flow matrix
        [
            [ 0.0025; 0.0025; 0.0025; 1.0025; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000 ] // I1
            [ 0.0050; 0.0050; 0.0050; 0.0050; 0.0050; 0.0050; 0.0050; 1.0050; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000 ] // I2
            [ 0.0065; 0.0065; 0.0065; 0.0065; 0.0065; 0.0065; 0.0065; 0.0065; 0.0065; 0.0065; 0.0065; 1.0065; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000 ] // I3
            [ 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 1.0085 ] // I4
        ]
    let t        = 4.0
    let result   = PtSmithWilson α UFR m C cashFlowDates [| t |]
    let df4      = result.[0,1]
    let nacaRate = dfToNaca t df4
    nacaRate |> should (equalWithin 0.000001) 0.03141

    
[<Test>]  
let ``should be able to generate indexes for use with 2D arrays and matrices``()=
    let expected = [ (0,0) ; (0,1) ; (1,0) ; (1,1) ]
    
    let indexes = Array2D.geneRatesIndexes 2 2 |> List.ofArray
    expected 
    |> List.zip indexes 
    |> List.iter  (fun (e, a) -> a |> should equal e)


[<Test>]  
let ``should be able to build 2D array from 1D Array``()=
    let input   = [| 1;2;3;4 |]
    let columns = 2
    let rows    = 2
    let output = Array.to2DArray rows columns input

    //let output = Array2D.init rows columns (fun i j -> input.[rows*i + j])
    let expected = array2D [ [ 1; 2]; [3; 4] ]
    output.GetLength(0) |> should equal 2
    output.GetLength(1) |> should equal 2
    output |> should equal expected 


[<Test>]  
let ``should be able to price all instruments back exactly``()=
    let α       = 0.1                                       // Mean reversion parameter controls rate at which curve reverts to UFR.
    let UFR     = 0.055                                     // Ultimate Forward Rate expressed in NACC
    let m, U, C = loadCurveInputs "..\sheets\ZaInputs.csv"  // Get market data.
    // Where:
    //
    // m is the market price vector
    // U is the vector of all cash flow dates
    // C is the cash flow matrix corresponding to m and U

    let Τ = U  // Get the discount factors at all of cash flow dates.
    
    // Build curve and get discount factors for the nodes.
    let discountFactors = (PtSmithWilson α UFR m C U Τ).Column 1
    let pricedBack      = (matrix C) * discountFactors

    // If we price back exactly then we should recover the market price vector 
    // Since Smith Wilson is prefect fit, we expect to get back the market
    // price vector to a high degree of precision.
    pricedBack 
    |> Seq.zip (vector (List.ofSeq m))  
    |> Seq.iter (fun (curvePrice, marketPrice) ->  curvePrice |> should (equalWithin 1e-11) marketPrice)
    
    

