[<AutoOpen>]
module StringUtils

open System
open System.Text.RegularExpressions

let private whiteSpaceRegex = new Regex(@"\s*", RegexOptions.Compiled) // This regex matches any type of white space including the tabs etc.

// Note that we can expose prepare string as stand alone function
// or an extension method.

/// Converts to upper case and removes all white space.
let perpareString (str:string) = whiteSpaceRegex.Replace(str.ToUpper(), String.Empty)

type String with // Extension method for string.
   /// Converts to upper case and removes all white space.
   member this.prepare() = perpareString(this) // Note how we can graft this function on as an extension method.

let toString obj =  obj.ToString()