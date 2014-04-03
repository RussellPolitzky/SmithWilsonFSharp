module TimedFunction

open System.Diagnostics

/// A timed version of a 6 parameter function.
let timedFunction6 f p1 p2 p3 p4 p5 p6 =
    let timer = Stopwatch.StartNew()
    let result = f p1 p2 p3 p4 p5 p6 
    result, (timer.ElapsedMilliseconds)

/// A timed version of a 5 parameter function.
let timedFunction5 f p1 p2 p3 p4 p5  =
    let timer = Stopwatch.StartNew()
    let result = f p1 p2 p3 p4 p5  
    result, (timer.ElapsedMilliseconds)

/// A timed version of a 1 parameter function.
let timedFunction2 f p1 =
    let timer = Stopwatch.StartNew()
    let result = f p1 
    result, (timer.ElapsedMilliseconds)