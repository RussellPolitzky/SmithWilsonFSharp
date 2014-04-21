module Quotes

open System
open Tenor

type ZeroQuoteDetail    = { EndTerm  : Tenor;                 Rate: float }
type ForwardQuoteDetail = { StartTerm: Tenor; EndTerm: Tenor; Rate: float }
type SwapQuoteDetail    = { EndTerm  : Tenor;                 Rate: float }

type Quote =
    | Zero    of ZeroQuoteDetail
    | Forward of ForwardQuoteDetail
    | Swap    of SwapQuoteDetail

let quotes = [
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
    ]

type CashFlowMatrixRow = { MarketPrice:float; Flows:(float * float)[] }

/// Gets the market price, all flows and accrual factors
/// associated with a given quote.
let getFlowsForQuote (basis:int) (baseDate:DateTime) (quote:Quote)  =
    let getYearFraction (tenor:Tenor)    = (float ((tenor.date baseDate) - baseDate).Days)/(float basis)
    let getΔ (d1:DateTime) (d2:DateTime) = (float ((d2-d1).Days))/(float basis)
    let getτ (d1:DateTime)               = getΔ baseDate d1 
     
    match quote with 

    | Zero(zd)    -> let τ    = getYearFraction zd.EndTerm
                     let r    = zd.Rate
                     let flow = 1.0 + r * τ
                     { MarketPrice = 1.0; Flows = [| (τ, flow) |] }

    | Forward(fd) -> let τ1   = getYearFraction fd.StartTerm
                     let τ2   = getYearFraction fd.EndTerm
                     let r    = fd.Rate
                     let flow = 1.0 + r*(τ2 - τ1) 
                     { MarketPrice = 0.0; Flows = [| (τ1, -1.0); (τ2, flow) |] }

    | Swap(sd)    -> let N           = 1.0 // Notional is 1
                     let fixedRate   = sd.Rate
                     let swapPeriods = sd.EndTerm.multiple() * 4
                     let endDate     = sd.EndTerm.date(baseDate)
                     let yearFractions = 
                         [| 0..swapPeriods |] 
                         |> Array.map (fun index -> (Month(-(swapPeriods-index)*3)).date(endDate))
                         |> Seq.pairwise
                         |> Seq.map (fun (d1, d2) -> getτ d2, getΔ d1 d2)
                     let flows = yearFractions 
                                 |> Seq.map (fun (τ, Δ) -> τ, N*Δ*fixedRate) 
                                 |> Array.ofSeq   
                     let (τ, flow) = flows.[flows.Length-1]  
                     let last = (τ, N + flow)
                     let flowsWithNominalRedemption = Array.concat [| flows.[0..swapPeriods-2]; [| last |] |] 
                     { MarketPrice = N; Flows = flowsWithNominalRedemption }

/// 
///
let getFlowsForQuotes (basis:int) (baseDate:DateTime) (quotes:Quote[]) = 
    quotes |> Array.map (fun quote -> getFlowsForQuote basis baseDate quote)

let getSortedAccrualFactors (flows:CashFlowMatrixRow[]) = 
    flows 
    |> Array.collect (fun cashFlowRecord -> cashFlowRecord.Flows |> Array.map (fun accrualFactorAndFlow -> fst accrualFactorAndFlow))
    |> Set.ofArray
    |> Array.ofSeq
    |> Array.sort