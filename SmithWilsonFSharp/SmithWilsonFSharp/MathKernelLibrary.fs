module MathKernelLibrary

open MathNet.Numerics.Algorithms.LinearAlgebra.Mkl
open MathNet.Numerics.Algorithms.LinearAlgebra

/// Enable the Intel MKL library.
let enableMKL() = 
    System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ // Won't load assemblies in FSI unless this is set - can't find assemblies.
    MathNet.Numerics.Control.LinearAlgebraProvider <- MklLinearAlgebraProvider()

/// Disable the Intel MKL library.
let disableMKL() = MathNet.Numerics.Control.LinearAlgebraProvider <- ManagedLinearAlgebraProvider()