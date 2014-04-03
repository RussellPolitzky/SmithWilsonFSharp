module Plot

open System
open FSharp.Charting
open FSharp.Charting.ChartTypes
open System.Drawing

let dashGrid = Grid(LineColor = Color.Gainsboro, LineDashStyle = System.Windows.Forms.DataVisualization.Charting.ChartDashStyle.Dash)

/// Plots a simple line graph.  Input is given as a 
/// sequence of float tuples.  The supplied multiplier
/// will be applied to all y values before they're 
/// plotted.              
let plot (values:(float * float) seq) multiplier title ytitle  = 
    let chart = values
                |> Seq.map (fun (term, value) -> term, value*multiplier)
                |> Chart.Line
                |> Chart.WithXAxis(MajorGrid = dashGrid, MinorGrid = dashGrid, Title="Term [Y]")
                |> Chart.WithYAxis(MajorGrid = dashGrid, MinorGrid = dashGrid, Title=ytitle)
                |> Chart.WithTitle(Text = title, FontSize = 11.0, InsideArea=false)
    chart.ShowChart() |> ignore 