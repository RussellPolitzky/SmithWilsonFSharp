module Tenor

open System
open System.Text.RegularExpressions

[<Literal>]
let daysInWeek = 7 // This declares a compile time constant.

let private tenorRegex = new Regex(@"(\d{1,5})([YMWD])", RegexOptions.Compiled)

type Tenor =
    | Year  of int
    | Month of int
    | Week  of int
    | Day   of int
    static member fromString (s:string) =
                let regMatch = tenorRegex.Match(s.prepare())
                if not regMatch.Success then failwith (sprintf "%s is not a valid tenor string.  Tenor strings should be of the form 2Y or 23M or 340D or 3W." s)
                let tenorType     = regMatch.Groups.Item(2).Value
                let tenorMultiple = int (regMatch.Groups.Item(1).Value)
                match tenorType with
                | "Y" -> Year tenorMultiple
                | "M" -> Month tenorMultiple
                | "W" -> Week tenorMultiple
                | "D" -> Day tenorMultiple
                | _   -> failwith (sprintf "%s is not a valid tenor string" s)
    override this.ToString() =
        match this with     
        | Year(n)  -> sprintf "%iY" n
        | Month(n) -> sprintf "%iM" n
        | Week(n)  -> sprintf "%iW" n
        | Day(n)   -> sprintf "%iD" n
    member this.date (baseDate:DateTime) =
        match this with 
        | Year(n)  -> baseDate.AddYears(n)
        | Month(n) -> baseDate.AddMonths(n)
        | Week(n)  -> baseDate.AddDays(float (daysInWeek*n))
        | Day(n)   -> baseDate.AddDays(float n)
    member this.multiple() = 
        match this with
        | Year (n) -> n
        | Month(n) -> n
        | Week (n) -> n
        | Day  (n) -> n
    member this.withMultiple newMultiple = 
        match this with
        | Year (n) -> Year  (newMultiple)
        | Month(n) -> Month (newMultiple)
        | Week (n) -> Week  (newMultiple)
        | Day  (n) -> Day   (newMultiple)
    member this.reverse() =
        this.withMultiple (-1*this.multiple())