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


/// Gets Cashflow Matrix Records for an array of quotes.
let getFlowsForQuotes (basis:int) (baseDate:DateTime) (quotes:Quote[]) = 
    quotes |> Array.map (fun quote -> getFlowsForQuote basis baseDate quote)


/// Returns an array of sorted accrual factors for 
/// the given array of CashFlowMatrixRecords.  The
/// return array is sorted in ascending order.
let getSortedAccrualFactors (flows:CashFlowMatrixRow[]) = 
    flows 
    |> Array.collect (fun cashFlowRecord -> cashFlowRecord.Flows |> Array.map (fun accrualFactorAndFlow -> fst accrualFactorAndFlow))
    |> Set.ofArray
    |> Array.ofSeq
    |> Array.sort


/// Gets the market prices vector,
///      the accrual factors vector (year fractions) and
///      the cash flow matrix
let buildMarketDataVectorsAndCfMatrixFromQuotes (basis:int) (baseDate:DateTime) (quotes:Quote[]) =
 
    let flows                = getFlowsForQuotes basis baseDate quotes
    let sortedAccrualFactors = getSortedAccrualFactors flows
    let noOfInstruments      = flows.Length
    let noOfAccrualFactors   = sortedAccrualFactors.Length

    let accrualFactorToIndexMap =
        sortedAccrualFactors 
        |> Seq.mapi (fun index accrualFactor -> accrualFactor, index)
        |> Map.ofSeq

    let cfMatrix     = Array2D.create noOfInstruments noOfAccrualFactors 0.0
    let marketPrices = Array.create noOfInstruments 0.0

    flows |> Array.iteri (fun row cashFlowMatrixRecord -> 
                              let flows = cashFlowMatrixRecord.Flows  
                              let marketPrice = cashFlowMatrixRecord.MarketPrice
                              marketPrices.[row] <- marketPrice
                              flows |> Array.iter (fun flow -> 
                                                       let (accrualFactor, flowValue) = flow
                                                       let col = accrualFactorToIndexMap.[accrualFactor] 
                                                       cfMatrix.[row,col] <- flowValue))
    marketPrices, sortedAccrualFactors, cfMatrix

