namespace Fezcoro

module Countries =
    open System
    open Elmish
    open Feliz
    open Feliz.Recharts
    open Types

    type ChartDataList =
        { date: DateTime
          datestr: string
          confirmed: int
          deaths: int
          recovered: int }

    type ChartData =
        { cases: ChartDataList list }

    type State =
        { country: Country option
          status: CovidStatus
          cases: ChartDataList list
          loading: bool
          error: string option }

    type Msg =
        | SetCountry of Country option
        | SetCases of ChartDataList list
        | SetLoading of bool
        | SetStatus of CovidStatus
        | FetchCases
        | FailedToFetch of exn

    let init() =
        { country = None
          status = Confirmed
          cases = List.empty
          loading = false
          error = None }

    let private areaChartDataFolder current next =
        let date, cases = next

        let confirmed =
            cases
            |> List.filter (fun cases -> cases.Status = "confirmed")
            |> List.sumBy (fun case -> case.Cases)

        let deaths =
            cases
            |> List.filter (fun cases -> cases.Status = "deaths")
            |> List.sumBy (fun case -> case.Cases)

        let recovered =
            cases
            |> List.filter (fun cases -> cases.Status = "recovered")
            |> List.sumBy (fun case -> case.Cases)

        let updated =
            match current |> List.tryFindIndex (fun case -> case.date = date) with
            | Some _ ->
                current
                |> List.map (fun case ->
                    match case.date = date with
                    | true ->
                        { case with
                              confirmed = case.confirmed + confirmed
                              deaths = case.deaths + deaths
                              recovered = case.recovered + recovered }
                    | false -> case)
            | None ->
                { date = date
                  datestr = date.ToShortDateString()
                  confirmed = confirmed
                  deaths = deaths
                  recovered = recovered }
                :: current

        updated

    let private getAndFoldCountries country =
        promise {
            let! results = Endpoints.getCasesByCountryByStatus country Confirmed
            let confirmed =
                match results with
                | Ok cases -> cases
                | Error err ->
                    printfn "%A" err
                    failwith "Failed to fetch confirmed"
            let! results = Endpoints.getCasesByCountryByStatus country Deaths
            let deaths =
                match results with
                | Ok cases -> cases
                | Error err ->
                    printfn "%A" err
                    failwith "Failed to fetch deaths"
            let! results = Endpoints.getCasesByCountryByStatus country Recovered
            let recovered =
                match results with
                | Ok cases -> cases
                | Error err ->
                    printfn "%A" err
                    failwith "Failed to fetch recovered"

            let results =
                confirmed @ deaths @ recovered
                |> List.groupBy (fun case -> case.Date)
                |> List.fold areaChartDataFolder List.empty<ChartDataList>

            return results
        }

    let update (msg: Msg) (state: State) =
        match msg with
        | SetCountry country ->
            { state with country = country },
            Cmd.batch
                [ Cmd.ofMsg (SetLoading true)
                  Cmd.ofMsg FetchCases ]
        | SetCases cases -> { state with cases = cases }, Cmd.ofMsg (SetLoading false)
        | SetLoading loading -> { state with loading = loading }, Cmd.none
        | SetStatus status -> { state with status = status }, Cmd.none
        | FetchCases ->
            let country =
                match state.country with
                | Some country -> country.Slug
                | None -> ""

            state, Cmd.OfPromise.either getAndFoldCountries country SetCases FailedToFetch
        | FailedToFetch err ->
            printfn "%A" err
            { state with error = Some err.Message }, Cmd.none

    let private chart (props: ChartData) =
        Recharts.areaChart
            [ areaChart.data props.cases
              areaChart.children
                  [ Recharts.cartesianGrid [ cartesianGrid.strokeDasharray (3, 3) ]
                    Recharts.xAxis [ xAxis.dataKey "datestr" ]
                    Recharts.yAxis []
                    Recharts.label []
                    Recharts.legend []
                    Recharts.tooltip []
                    Recharts.area
                        [ area.monotone
                          area.dataKey "confirmed"
                          area.name "Confirmados"
                          area.stackId "1"
                          area.fill "#a14703"
                          area.stroke "#a14703" ]
                    Recharts.area
                        [ area.monotone
                          area.dataKey "deaths"
                          area.name "Decesos"
                          area.stackId "1"
                          area.fill "#780000"
                          area.stroke "#780000" ]
                    Recharts.area
                        [ area.monotone
                          area.dataKey "recovered"
                          area.name "Saludables"
                          area.stackId "1"
                          area.fill "#038a30"
                          area.stroke "#038a30" ] ] ]

    let casesChart =
        React.functionComponent
            ("CasesChart",
             (fun (props: ChartData) ->
                 Recharts.responsiveContainer
                     [ responsiveContainer.height 500
                       responsiveContainer.minWidth 320
                       responsiveContainer.debounce 250
                       responsiveContainer.chart <| chart props ]))

    let content (state: State) (dispatch: Msg -> unit) =
        let countryContent =
            match state.country with
            | Some _ ->
                let cases = state.cases |> List.sortBy (fun case -> case.date)
                casesChart { cases = cases }
            | None -> Html.div [ prop.text "Selecciona un pais" ]
        Html.section
            [ if state.loading then Html.progress [ prop.custom ("indeterminate", "true") ]
              countryContent ]

    let render (state: State) (dispatch: Msg -> unit) = content state dispatch
