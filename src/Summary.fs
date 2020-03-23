namespace Fezcoro

module Summary =
    open System
    open Elmish
    open Feliz
    open Feliz.Recharts
    open Endpoints
    open Types

    type ChartData =
        { summary: Summary }

    type State =
        { country: Country option
          summaryList: Summary list
          date: DateTime
          error: string option }

    type Msg =
        | SetCountry of Country option
        | SetSummary of {| Countries: Summary list; Date: DateTime |}
        | FetchSummary
        | FailedToFetch of exn

    let init() =
        { country = None
          summaryList = List.empty
          date = DateTime.Today
          error = None }, Cmd.ofMsg FetchSummary

    let update (msg: Msg) (state: State) =
        match msg with
        | SetCountry country -> { state with country = country }, Cmd.none
        | SetSummary summary ->
            { state with
                  summaryList = summary.Countries
                  date = summary.Date }, Cmd.none
        | FetchSummary ->
            let summaryP() =
                promise {
                    let! results = getSummary
                    return match results with
                           | Ok summary -> summary
                           | Error err ->
                               printfn "%A" err
                               failwith "Failed to Fetch Summary"
                }
            state, Cmd.OfPromise.either summaryP () SetSummary FailedToFetch
        | FailedToFetch ex -> { state with error = Some ex.Message }, Cmd.none

    let private chart (props: ChartData) =
        Recharts.barChart
            [ barChart.data [ props.summary ]
              barChart.children
                  [ Recharts.cartesianGrid [ cartesianGrid.strokeDasharray (3, 3) ]
                    Recharts.xAxis [ xAxis.dataKey props.summary.Country ]
                    Recharts.yAxis []
                    Recharts.label []
                    Recharts.legend []
                    Recharts.tooltip []
                    Recharts.bar
                        [ bar.dataKey "NewConfirmed"
                          bar.name "Confirmados (Nuevos)"
                          bar.fill "#a14703" ]

                    Recharts.bar
                        [ bar.dataKey "TotalConfirmed"
                          bar.fill "#ed9702"
                          bar.name "Total Confirmados" ]
                    Recharts.bar
                        [ bar.dataKey "NewDeaths"
                          bar.name "Decesos (Nuevos)"
                          bar.fill "#d11919" ]

                    Recharts.bar
                        [ bar.dataKey "TotalDeaths"
                          bar.name "Total Decesos"
                          bar.fill "#780000" ]

                    Recharts.bar
                        [ bar.dataKey "NewRecovered"
                          bar.name "Saludables (Nuevos)"
                          bar.fill "#038a66" ]

                    Recharts.bar
                        [ bar.dataKey "TotalRecovered"
                          bar.name "Total Saludables"
                          bar.fill "#038a30" ] ] ]

    let summaryChart =
        React.functionComponent
            ("SummaryChart",
             (fun (props: ChartData) ->
                 Recharts.responsiveContainer
                     [ responsiveContainer.height 500
                       responsiveContainer.minWidth 320
                       responsiveContainer.debounce 250
                       responsiveContainer.chart <| chart props ]))

    let content (state: State) (dispatch: Msg -> Unit) =
        let summaryContent =
            match state.country with
            | Some country ->
                let selected = state.summaryList |> List.find (fun summary -> summary.Country = country.Country)
                summaryChart { summary = selected }
            | None -> Html.div [ prop.text "Selecciona un pais" ]
        Html.section
            [ Html.header
                [ prop.children
                    [ Html.h3 [ prop.text (sprintf "Actualizado: %s" (state.date.ToShortDateString())) ] ] ]
              summaryContent ]

    let render (state: State) (dispatch: Msg -> unit) = content state dispatch
