module ZaCurveQuotes

open System
open Tenor
open Quotes

// ZA quotes for swap curve on 2011-02-10 from Paul Du Preez's thesis
let basis = 365
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