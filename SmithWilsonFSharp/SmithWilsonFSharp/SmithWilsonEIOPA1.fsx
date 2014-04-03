
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
let cashFlowDates = [| 1.0; 2.0; 3.0; 4.0; 5.0 |] 

let C = // Cash flow matrix
        [
           [  1.01;  0.00;  0.00;  0.00;  0.00 ] // instrument 1
           [  0.02;  1.02;   0.0;   0.0;   0.0 ] // instrument 2
           [ 0.026; 0.026; 1.026;   0.0;   0.0 ] // instrument 3
           [ 0.034; 0.034; 0.034; 0.034; 1.034 ] // instrument 4
        ]

let time    = [| 0.0..(1.0/365.0)..5.0 |] 
let timeDfs = PtSmithWilson α UFR m C cashFlowDates time
let dfs     = timeDfs.Column(1)

let rateNACA = getNacsRates time dfs 
let rateSimp = getSimpRates time dfs 
let fwdRates = getFwdRates time dfs
    
plot (dfs      |> Seq.zip time)   1.0 "EIOPA Sample : Discount Factors" "Discount Factor"
plot (rateNACA |> Seq.zip time) 100.0 "EIOPA Sample : NACA Rates"       "Rate     [% NACA]"
plot (rateSimp |> Seq.zip time) 100.0 "EIOPA Sample : SIMP Rates"       "Rate     [% SIMP]"
plot fwdRates                   100.0 "EIOPA Sample : 1 DAY FWD Rates"  "Rate     [% SIMP]"