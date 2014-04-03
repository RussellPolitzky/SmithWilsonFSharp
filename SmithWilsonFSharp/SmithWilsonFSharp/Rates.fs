module Rates

// Convert discount factors to discrete rates.
let dfToDiscreteRate m τ df = 
    if τ = 0.0 then 0.0 // can't get interest for zero time.
    else m*((1.0/(df**(1.0/(m*τ)))-1.0))

let dfToNaca = dfToDiscreteRate 1.0
let dfToNacs = dfToDiscreteRate 2.0
let dfToNacq = dfToDiscreteRate 4.0
let dfToNacm = dfToDiscreteRate 12.0

/// Convert discount factors to simple rate
let dfToSimp τ df = 
    if τ = 0.0 then 0.0 // can't get interest for zero time.
    else (1.0/τ)*(1.0/df - 1.0)

/// Calculate a SIMP forward rate given 2 discount factors 
/// and the year fraction between them 
let getFwdRate τ1 df1 τ2 df2 = 
    let Δ = τ2-τ1
    (1.0/Δ)*(df1/df2-1.0)

/// Find SIMP forward rates given terms 
/// and discount factors.
let getFwdRates terms discountfactors = 
    (terms |> Seq.zip discountfactors) 
    |> Seq.pairwise
    |> Seq.map (fun ((df1, τ1), (df2 ,τ2)) -> let fwdRate = getFwdRate τ1 df1 τ2 df2
                                              (τ1, fwdRate))

let private getDiscreteRates (toDiscreteRate:float->float->float) terms discountfactors = 
    terms 
    |> Seq.zip discountfactors
    |> Seq.map (fun (df,τ) -> toDiscreteRate τ df)
    |> List.ofSeq

/// Get simple rates for the given terms 
/// and discount factors.
let getSimpRates terms discountfactors = 
    terms 
    |> Seq.zip discountfactors 
    |> Seq.map (fun (df,τ) -> dfToSimp τ df) 
    |> List.ofSeq

/// Gets NACA rate for a given τ and discount factor
let getNacaRates τ df = getDiscreteRates dfToNaca τ df
/// Gets NACS rate for a given τ and discount factor
let getNacsRates τ df = getDiscreteRates dfToNacs τ df
/// Gets NACQ rate for a given τ and discount factor
let getNacqRates τ df = getDiscreteRates dfToNacq τ df
/// Gets NACM rate for a given τ and discount factor
let getNacmRates τ df = getDiscreteRates dfToNacm τ df

type DfSource = float[] -> (float * float)[] 

let getFwdRatesBetweenStartAndEndDates (dfSource:DfSource) (startAndEndDates:(float * float)[]) =
    let allDfs = [| for (ts, te) in startAndEndDates do yield ts; yield te |] 
                 |> Set.ofArray // unique set of dates
                 |> Set.toArray // df source takes arrays
                 |> dfSource    // find dfs in bulk 
                 |> Map.ofArray // map for lookup.
    startAndEndDates 
    |> Array.Parallel.map (fun (ts, te) -> let Δ   = te - ts
                                           let df1 = allDfs.[ts] 
                                           let df2 = allDfs.[te]
                                           getFwdRate ts df1 te df2)

