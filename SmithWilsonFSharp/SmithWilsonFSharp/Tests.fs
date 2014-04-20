module SmithWilsonFSharp.Tests

open FsUnit
open NUnit.Framework
open System
open SmithWilson
open Rates
open Tenor
open Quotes
open System.IO
open ArrayExtensions
open LoadMarketData
open MathKernelLibrary
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

    // If the curve is a perfect fit then:
    // 
    // C * d = m 
    //
    // Where :
    //
    // C is the cashflow matrix
    // d is a vector of discount factors supplied by the Smith-Wilson curve
    // m is the market price vector
    //
    // This equation can be used to test the precision of 
    // of the Smith Wilson algorithm.  Specifically, comparing m, 
    // as calculated above to the given market price vector 
    // test to see of the input instrument prices back perfectly.

    let α       = 0.1                                       // Mean reversion parameter controls rate at which curve reverts to UFR.
    let UFR     = 0.055                                     // Ultimate Forward Rate expressed in NACC
    let m, U, C = loadCurveInputs "..\sheets\ZaInputs.csv"  // Get market data.
    // Where:
    //
    // m is the market price vector
    // U is the vector of all cash flow dates
    // C is the cash flow matrix corresponding to m and U

    let Τ = U  // Get the discount factors at all of cash flow dates.

    // Build curve and get discount factors for the cash flow dates.
    let discountFactors = (PtSmithWilson α UFR m C U Τ).Column 1
    let recoveredMarketVector = (matrix C) * discountFactors

    // If we price back exactly then we should recover the market price vector. 
    // Since Smith Wilson is prefect fit, we expect to get back the market
    // price vector to a high degree of precision.

    let maxDifference = abs ((recoveredMarketVector - (vector (List.ofSeq m))).Maximum())
    maxDifference |> should be (lessThan 1e-11)


[<Test>]  
let ``should be able to get flows for zero quote``()=
    let basis    = 365
    let baseDate = DateTime(2011, 02, 10)
    let quote    = Zero({EndTerm=Day(1); Rate = 0.0528})
    let { MarketPrice = marketPrice ; Flows = flows} = getFlowsForQuote basis quote baseDate
    marketPrice |> should equal 1.0
    fst flows.Head |> should (equalWithin 1e-12) 0.002739726027397
    snd flows.Head |> should (equalWithin 1e-12) 1.000144657534250

let compareTuple (tpl1:(float * float)) (tpl2:(float * float)) =
    fst tpl1 |> should (equalWithin 1e-12) (fst tpl2)
    snd tpl1 |> should (equalWithin 1e-12) (snd tpl2)


[<Test>]  
let ``should be able to get flows for forward quote``()=
    let expected = 
           [
                (0.076712328767123, -1.000000000000000)
                (0.328767123287671,  1.014115068493150)
           ]
    let basis    = 365
    let baseDate = DateTime(2011, 02, 10)
    let quote    = Forward({StartTerm=Month(1); EndTerm=Month(4); Rate = 0.0560})
    let { MarketPrice = marketPrice ; Flows = flows} = getFlowsForQuote basis quote baseDate
    marketPrice |> should equal 0.0
    compareTuple expected.[0] flows.[0]
    compareTuple expected.[1] flows.[1]


[<Test>]  
let ``should be able to get flows for swap quote``()=
    let expected = 
        [
            0.243835616438356, 0.015898082191781
            0.495890410958904, 0.016433972602740
            0.747945205479452, 0.016433972602740
            1.000000000000000, 0.016433972602740
            1.246575342465750, 0.016076712328767
            1.498630136986300, 0.016433972602740
            1.750684931506850, 0.016433972602740
            2.002739726027400, 1.016433972602740
        ]

    let basis    = 365
    let baseDate = DateTime(2011, 02, 10)
    let quote    = Swap({EndTerm=Year(2); Rate = 0.0652})
    let { MarketPrice = marketPrice ; Flows = flows} = getFlowsForQuote basis quote baseDate
    marketPrice |> should equal 1.0    
    expected 
    |> Seq.zip flows 
    |> Seq.iter (fun (exp, actual) -> compareTuple exp actual)

