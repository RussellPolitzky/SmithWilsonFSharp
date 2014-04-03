
#r "System.Windows.Forms"
#r "System.Windows.Forms.DataVisualization"
#r @"..\packages\MathNet.Numerics.2.6.2\lib\net40\MathNet.Numerics.dll"
#r @"..\packages\MathNet.Numerics.FSharp.2.6.0\lib\net40\MathNet.Numerics.FSharp.dll"
#r @"..\packages\FSharp.Charting.0.90.5\lib\net40\FSharp.Charting.dll"
#load @"Rates.fs"
#load @"SmithWilson.fs"
#load @"Plot.fs"

open System
open FSharp.Charting
open FSharp.Charting.ChartTypes
open System.Drawing
open Rates
open SmithWilson
open Plot

// This is the first of the EIOPA examples from the paper at the following URL
// http://eiopa.europa.eu/fileadmin/tx_dam/files/consultations/QIS/QIS5/ceiops-paper-extrapolation-risk-free-rates_en-20100802.pdf

let α   = 0.1                    // Mean reversion parameter controls rate at which curve reverts to UFR.
let UFR = Math.Log(1.0 + 0.042)  // Ultimate Forward Rate expressed in NACC (converted from 0.042 NACA)

// Market data.
let m = [| 1.0; 1.0; 1.0; 1.0 |] // market prices

// Cash flow dates in year fractions.
// This is the union of all of the cash flow dates
// from all instruments.  It's expressed in years
// here.
let cashFlowDates = [ 0.25 .. 0.25 .. 5.0 ] |> Array.ofList 
let C = // Cash flow matrix
        [
            [ 0.0025; 0.0025; 0.0025; 1.0025; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000 ] // I1
            [ 0.0050; 0.0050; 0.0050; 0.0050; 0.0050; 0.0050; 0.0050; 1.0050; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000 ] // I2
            [ 0.0065; 0.0065; 0.0065; 0.0065; 0.0065; 0.0065; 0.0065; 0.0065; 0.0065; 0.0065; 0.0065; 1.0065; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000 ] // I3
            [ 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 0.0085; 1.0085 ] // I4
        ]

let time    = [| 0.0..(1.0/365.0)..5.0 |] 
let TimeDfs = PtSmithWilson α UFR m C cashFlowDates time
let dfs     = TimeDfs.Column(1)

let rateNACA = getNacaRates time dfs 
let rateSimp = getSimpRates time dfs 
let fwdRates = getFwdRates time dfs
    
plot (dfs      |> Seq.zip time)   1.0 "EIOPA Sample : Discount Factors" "Discount Factor"
plot (rateNACA |> Seq.zip time) 100.0 "EIOPA Sample : NACA Rates"       "Rate     [% NACA]"
plot (rateSimp |> Seq.zip time) 100.0 "EIOPA Sample : SIMP Rates"       "Rate     [% SIMP]"
plot fwdRates                   100.0 "EIOPA Sample : 1 DAY FWD Rates"  "Rate     [% SIMP]"