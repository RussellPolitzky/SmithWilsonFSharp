// This is the first of the EIOPA examples from the paper at the following URL
// http://eiopa.europa.eu/fileadmin/tx_dam/files/consultations/QIS/QIS5/ceiops-paper-extrapolation-risk-free-rates_en-20100802.pdf

#r "System.Windows.Forms"
#r "System.Windows.Forms.DataVisualization"
#r @"..\packages\MathNet.Numerics.2.6.2\lib\net40\MathNet.Numerics.dll"
#r @"..\packages\MathNet.Numerics.FSharp.2.6.0\lib\net40\MathNet.Numerics.FSharp.dll"
#r @"..\packages\FSharp.Charting.0.90.5\lib\net40\FSharp.Charting.dll"
#load @"Array.fs"
#load @"Rates.fs"
#load @"SmithWilson.fs"
#load @"Plot.fs"
#load @"MathKernelLibrary.fs"
#load @"LoadMarketData.fs"
#load @"TimedFunction.fs"

open System
open System.IO
open FSharp.Charting
open FSharp.Charting.ChartTypes
open System.Drawing
open System.Text
open System.Diagnostics
open System.Linq
open System.Collections.Generic

open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.LinearAlgebra.Double
open DenseMatrix
open Matrix

open Rates
open SmithWilson
open Plot
open MathKernelLibrary
open LoadMarketData
open TimedFunction

#time "on" // turn on timing in the FSI.


let getForwards (Τ:Vector<float>) (dfs:Vector<float>) days = 
    let dfSource = 
        let dfMap = Seq.zip Τ dfs |> Map.ofSeq
        fun (times:float[]) -> times |> Array.map (fun t -> t, dfMap.[t]) 
    let startAndEndDates = [| for i = 0 to (dfs.Count-days-1) do yield i, i+days |] 
                           |> Array.map (fun (startIndex, endIndex) -> Τ.[startIndex], Τ.[endIndex])
    getFwdRatesBetweenStartAndEndDates dfSource startAndEndDates


// In this case, we're not concerned about the
// extrapolation beyond the liquid instruments,
// so following two parameters are not important.
// Also they will have only a small effect on 
// the curve and in all cases it will always be 
// a perfect fit, regardless of what we choose
// these to be.

let α       = 0.1                                       // Mean reversion parameter controls rate at which curve reverts to UFR.
let UFR     = 0.055                                     // Ultimate Forward Rate expressed in NACC
// Smith-Wilson produces a curve which 

let m, U, C = loadCurveInputs "..\sheets\ZaInputs.csv"  // Get market data.
// Where:
//
// m is the market price vector
// U is the vector of all cash flow dates
// C is the cash flow matrix corresponding to m and U
     
enableMKL()  
let lastDate = U.[U.Length-1]
let time     = [| 0.0..(1.0/365.0)..lastDate |] 

// Plot to 100 years to observe the reversion to 
// the ultimate forward rate.
//let time     = [| 0.0..(1.0/365.0)..100.0 |] 

// Build curve
let smithWilsonCurve, curveFittingTime  = timedFunction5 PtSmithWilson α UFR m C U 
// Get discount factors off the curve.
let resultMatrix, discounFactorCalcTime = timedFunction2 smithWilsonCurve time

printfn "Curve fitting time: %ims." curveFittingTime 
printfn "Discount factor calculation time: %ims." discounFactorCalcTime 

let dfs = resultMatrix.Column 1
let Τ   = resultMatrix.Column 0

let threeMonthForwards = getForwards  Τ dfs 90
let rateNACM           = getNacmRates time dfs 
let rateSimp           = getSimpRates time dfs 
let fwdRates           = getFwdRates  time dfs
    
plot (dfs      |> Seq.zip Τ)   1.0 "ZA Sample : Discount Factors"                  "Discount Factor"
plot (rateNACM |> Seq.zip Τ) 100.0 "ZA Sample : NACM Rates"                        "Rate     [% NACM]"
plot (rateSimp |> Seq.zip Τ) 100.0 "ZA Sample : SIMP Rates"                        "Rate     [% SIMP]"
plot fwdRates                100.0 "ZA Sample : Predicted ON Rates (1D Forwards)"  "Rate     [% SIMP]"
plot (threeMonthForwards |> Seq.zip Τ) 100.0 "ZA Sample : 3M Forwards"  "Rate     [% SIMP]"