module SmithWilson

open System
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Double.Vector
open System.IO
open Array.Parallel

// This implements the Smith Wilson yield curve
// fitter as per the EIOPA paper given below.
//
// http://eiopa.europa.eu/fileadmin/tx_dam/files/consultations/QIS/QIS5/ceiops-paper-extrapolation-risk-free-rates_en-20100802.pdf
//
/// Returns a lambda function which is the Smith-Wilson curve.
/// The lambda takes a vector of year fractions, for which it  
/// returns corresponding discount factors.
/// 
/// α   - mean reversion coefficient, 
/// UFR - ultimate forward rate, 
/// m   - market price vector,
/// CT  - transposed cash flow matrix,
/// U   - array of cash flow dates given as year fractions 
///       > curve inception date e.g. [| 0.5; 1.0; 2.0; 5.0 ... |]
///
#nowarn "49" // Don't bug me about the the UFR variable name.
let PtSmithWilson α UFR marketPrices (C:float list list) (U:float[]) =

    /// Wilson kernel function.
    /// http://www.HostMath.com/Show.aspx?Code=%0AW%5Cleft(%20t%2C%20u_j%20%20%5Cright)%20%3D%0Ae%5E%7B-UFR(t%2Bu_j)%7D%0A%5Cleft(%0A%5Calpha%20%5Cmin(t%2Cu_j)%0A-%5Cfrac%7B1%7D%7B2%7D%0Ae%5E%7B-%5Calpha%20%5Cmax(t%2Cu_j)%7D%0A%5Cleft%5B%0Ae%5E%7B%5Calpha%20%5Cmin(t%2Cu_j)%7D-e%5E%7B-%5Calpha%20%5Cmin(t%2Cu_j)%7D%0A%5Cright%5D%0A%5Cright)%0A
    let Wilson (α:float) UFR (t:float) (uj:float) = 
        exp(-UFR * (t + uj)) *
        (
            α*(min t uj) 
            - 0.5*exp(-α * (max t uj)) 
            * ( exp(α*(min t uj)) - exp(-α*(min t uj)) )
        )

    // Solving for the Wilson function coefficients is equivalent
    // to bootstrapping in the sense that once this is done, we're
    // in a position to find any discount factor off the curve whether
    // it be on a node, between nodes or beyond the liquid terms (extrapolated).
    // Smith-Wilson is desgined to have predicatable behaviour beyond the 
    // liquid region.  This behaviour is controlled by α, the mean reversion
    // coefficient and UFR, the ultimate, continuously compounded forward 
    // rate.
    let solveForWilsonFunctionWeights numberOfFlows (C:Generic.Matrix<float>) (CT:Generic.Matrix<float>) (m:Generic.Vector<float>) = 
        let W = DenseMatrix.init numberOfFlows numberOfFlows (fun i j -> Wilson α UFR U.[i] U.[j])
        let μ = U |> Array.map (fun ui -> exp(-UFR*ui)) |> DenseVector.raw 
        let ξ = (C*W*CT).Inverse()*(m-C*μ)           // Wilson function weights
        ξ

    // Curve 'stripping'
    let C                   = matrix C // initialise a Math.Net matrix from a list of lists
    let CT                  = C.Transpose()
    let m                   = DenseVector.raw marketPrices // initialize a Math.Net vector from an array
    let numberOfFlows       = U.Length
    let numberOfInstruments = m.Count
    let ξ                   = solveForWilsonFunctionWeights numberOfFlows C CT m

    // Discount Factors
    // MWT => the Matrix WT
    // T   - array of year fractions for which discount factors 
    //       are required e.g. [| (1.0/365); (30.0/365.0); ... 10.0; 15.0; 20.0 |]
    fun (T:float[]) ->  
        let MWT  = T |> Array.Parallel.map (fun τ -> U |> Array.map (fun ui -> Wilson α UFR τ ui)) // Parallelize over df dates to build MWT matrix
                     |> DenseMatrix.ofRows T.Length U.Length                                       // Each row in the matrix is one WT vector from the Eiopa paper.
        let E    = DenseVector.raw (T |> Array.Parallel.map(fun τ -> exp(-UFR*τ)))                 // As many as 10000 τ for 1 day resolution out to 30Y.
        let dfs  = E + MWT*CT*ξ  
        let TVec = DenseVector.raw T
        DenseMatrix.ofColumnVectors [TVec; DenseVector.OfVector(dfs)]


    // The code under the "Discount Factors" section above, 
    // is what gets executed to find those 
    // corresponding to the given times in the T vector (There 
    // may be several thousand elements in this vector).

    // We differ slightly from the Eiopa paper here to be able to 
    // get multiple discount factors from a single call.  In other words, 
    // this is a vectorised version of what we see in the paper 
    // (ie. we build a WT matrix rather than a vector and we build
    // a vector of ultimate forward rate terms.
    //
    // http://www.HostMath.com/Show.aspx?Code=%7B%5Cbf%20E%7D%2B%7B%5Cbf%20M%7D_%7B%7B%5Cbf%20W%7D%5ET%7D%7B%5Cbf%20C%7D%5ET%7B%5Cbf%20%5Czeta%7D
    // Where 
    //   E   is the vector of values from the UFR term e^(-UFR*t) and
    //   MWT is the matrix whos rows are the WT vectors from the Eiopa paper.
    //   t   in the context of this function is the year fraction with
    //       t=0 being the curve reference date, t=1.0 for example, being 
    //       1 year on from the curve reference date. 