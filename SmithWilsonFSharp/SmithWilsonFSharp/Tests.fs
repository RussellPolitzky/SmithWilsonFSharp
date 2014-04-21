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
open TestUtils
open MathKernelLibrary
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Double.Vector


[<Test>]  
let ``01 should be able to match EIOPA sample 1 rate``()=
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
let ``02 should be able to match EIOPA sample 2 rate``()=
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
let ``03 should be able to generate indexes for use with 2D arrays and matrices``()=
    let expected = [ (0,0) ; (0,1) ; (1,0) ; (1,1) ]
    
    let indexes = Array2D.geneRatesIndexes 2 2 |> List.ofArray
    expected 
    |> List.zip indexes 
    |> List.iter  (fun (e, a) -> a |> should equal e)


[<Test>]  
let ``04 should be able to build 2D array from 1D Array``()=
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
let ``05 should be able to price all instruments back exactly``()=

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
    

[<Literal>]
let basis = 365
let baseDate = DateTime(2011, 02, 10)


[<Test>]  
let ``06 should be able to get flows for zero quote``()=
    let quote    = Zero({EndTerm=Day(1); Rate = 0.0528})
    let { MarketPrice = marketPrice ; Flows = flows} = getFlowsForQuote basis baseDate quote 
    marketPrice |> should equal 1.0
    fst flows.[0] |> should (equalWithin 1e-12) 0.002739726027397
    snd flows.[0] |> should (equalWithin 1e-12) 1.000144657534250


[<Test>]  
let ``07 should be able to get flows for forward quote``()=
    let expected = 
           [
                (0.076712328767123, -1.000000000000000)
                (0.328767123287671,  1.014115068493150)
           ]
    let quote    = Forward({StartTerm=Month(1); EndTerm=Month(4); Rate = 0.0560})
    let { MarketPrice = marketPrice ; Flows = flows} = getFlowsForQuote basis baseDate quote 
    marketPrice |> should equal 0.0
    compareTuple expected.[0] flows.[0]
    compareTuple expected.[1] flows.[1]


[<Test>]  
let ``08 should be able to get flows for swap quote``()=
    let expected = 
        [|
            0.243835616438356, 0.015898082191781 // These from the original Excel model.
            0.495890410958904, 0.016433972602740
            0.747945205479452, 0.016433972602740
            1.000000000000000, 0.016433972602740
            1.246575342465750, 0.016076712328767
            1.498630136986300, 0.016433972602740
            1.750684931506850, 0.016433972602740
            2.002739726027400, 1.016433972602740
        |]
    let quote    = Swap({EndTerm=Year(2); Rate = 0.0652})
    let { MarketPrice = marketPrice ; Flows = flows} = getFlowsForQuote basis baseDate quote 
    marketPrice |> should equal 1.0    
    expected 
    |> Array.zip flows 
    |> Array.iter (fun (exp, actual) -> compareTuple exp actual)


let sampleQuotes = 
    [|
        Zero   ({ EndTerm   = Day  (1)                  ; Rate = 0.0528 })
        Forward({ StartTerm = Month(1); EndTerm=Month(4); Rate = 0.0560 })
        Swap   ({ EndTerm   = Year (2)                  ; Rate = 0.0652 })
    |]


[<Test>]  
let ``09 should be able to get all accrual factors from quotes``()=
    let expected = 
        [|
          0.0027397260273972603 // 1D
          0.076712328767123292  // 1M
          0.24383561643835616   // 3M
          0.32876712328767121   // 4M
          0.49589041095890413   // 6M
          0.74794520547945209   // 9M
          1.0                   // 12M
          1.2465753424657535    // 15M
          1.4986301369863013    // 18M
          1.7506849315068493    // 21M
          2.0027397260273974    // 2Y
        |]
    let flows                = getFlowsForQuotes       basis baseDate sampleQuotes
    let sortedAccrualFactors = getSortedAccrualFactors flows
    sortedAccrualFactors |> should equal expected


[<Test>]  
let ``10 should be able to get market data from quotes``()=

    let expectedCfMatrix = 
        array2D
           [
               [1.0001446575342465; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0]
               [0.0; -1.0; 0.0; 1.0141150684931506; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ;0.0]
               [0.0; 0.0; 0.015898082191780821; 0.0; 0.016433972602739724; 0.016433972602739724; 0.016433972602739724; 0.016076712328767121; 0.016433972602739724; 0.016433972602739724; 1.0164339726027398]
           ]

    let expectedAccrualFactors = 
        [|
          0.0027397260273972603 // 1D
          0.076712328767123292  // 1M
          0.24383561643835616   // 3M
          0.32876712328767121   // 4M
          0.49589041095890413   // 6M
          0.74794520547945209   // 9M
          1.0                   // 12M
          1.2465753424657535    // 15M
          1.4986301369863013    // 18M
          1.7506849315068493    // 21M
          2.0027397260273974    // 2Y
        |]

    let expectedMarketPrices = [ 1.0; 0.0; 1.0 ]
    
    let marketPrices, sortedAccrualFactors, cfMatrix = buildMarketDataVectorsAndCfMatrixFromQuotes basis baseDate sampleQuotes
     
    marketPrices         |> should equal expectedMarketPrices
    sortedAccrualFactors |> should equal expectedAccrualFactors
    cfMatrix             |> should equal expectedCfMatrix


// ZA quotes for swap curve on 2011-02-10 from Paul Du Preez's thesis
let zaBaseDate = DateTime(2011, 02, 10)
let zaQuotes = [|
        Zero   ({                       EndTerm = Day  ( 1); Rate = 5.280 / 100.0 })
        Zero   ({                       EndTerm = Month( 1); Rate = 5.470 / 100.0 })
        Zero   ({                       EndTerm = Month( 3); Rate = 5.575 / 100.0 })
        Forward({StartTerm = Month( 1); EndTerm = Month( 4); Rate = 5.600 / 100.0 })
        Forward({StartTerm = Month( 2); EndTerm = Month( 5); Rate = 5.650 / 100.0 })
        Forward({StartTerm = Month( 3); EndTerm = Month( 6); Rate = 5.650 / 100.0 })
        Forward({StartTerm = Month( 4); EndTerm = Month( 7); Rate = 5.710 / 100.0 })
        Forward({StartTerm = Month( 5); EndTerm = Month( 8); Rate = 5.760 / 100.0 })
        Forward({StartTerm = Month( 6); EndTerm = Month( 9); Rate = 5.850 / 100.0 })
        Forward({StartTerm = Month( 7); EndTerm = Month(10); Rate = 5.890 / 100.0 })
        Forward({StartTerm = Month( 8); EndTerm = Month(11); Rate = 6.010 / 100.0 })
        Forward({StartTerm = Month( 9); EndTerm = Month(12); Rate = 6.160 / 100.0 })
        Forward({StartTerm = Month(12); EndTerm = Month(15); Rate = 6.620 / 100.0 })
        Forward({StartTerm = Month(15); EndTerm = Month(18); Rate = 7.060 / 100.0 })
        Forward({StartTerm = Month(18); EndTerm = Month(21); Rate = 7.500 / 100.0 })
        Swap   ({                       EndTerm = Year ( 2); Rate = 6.520 / 100.0 })
        Swap   ({                       EndTerm = Year ( 3); Rate = 7.130 / 100.0 })
        Swap   ({                       EndTerm = Year ( 4); Rate = 7.550 / 100.0 })
        Swap   ({                       EndTerm = Year ( 5); Rate = 7.850 / 100.0 })
        Swap   ({                       EndTerm = Year ( 6); Rate = 8.060 / 100.0 })
        Swap   ({                       EndTerm = Year ( 7); Rate = 8.210 / 100.0 })
        Swap   ({                       EndTerm = Year ( 8); Rate = 8.310 / 100.0 })
        Swap   ({                       EndTerm = Year ( 9); Rate = 8.380 / 100.0 })
        Swap   ({                       EndTerm = Year (10); Rate = 8.420 / 100.0 })
        Swap   ({                       EndTerm = Year (12); Rate = 8.460 / 100.0 })
        Swap   ({                       EndTerm = Year (15); Rate = 8.450 / 100.0 })
        Swap   ({                       EndTerm = Year (20); Rate = 8.370 / 100.0 })
        Swap   ({                       EndTerm = Year (25); Rate = 8.290 / 100.0 })
        Swap   ({                       EndTerm = Year (30); Rate = 8.150 / 100.0 })
    |]


[<Test>]  
let ``11 should generate market vectors and matrices as per original sheet``()=
    let me, Ue, Ce = loadCurveInputs "..\sheets\ZaInputs.csv"  // Get market data.
    let ma, Ua, Ca = buildMarketDataVectorsAndCfMatrixFromQuotes basis zaBaseDate zaQuotes
    ma |> should equal me
    Ua |> Seq.zip Ue |> Seq.iter(fun (ua, ue) -> ua |> should (equalWithin 1e-12) ue)
    Ca|> arrayShouldEqual 1e-12 (array2D Ce) // Ce is a list of lists of floats

