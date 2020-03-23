namespace Fezcoro

module App =

    open Elmish
    open Feliz
    open Feliz.Router
    open Types
    open Endpoints

    type Page =
        | Summary
        | Country
        | Province

    type State =
        { countries: Country list
          selected: Country option
          error: string option
          currentUrl: string list
          summaryState: Summary.State
          countriesState: Countries.State }

    type Msg =
        | FetchCountries
        | SetCountries of Country list
        | SetSelected of Country option
        | FailedToFetch of exn
        | UrlChanged of string list
        | SummaryMsg of Summary.Msg
        | CountriesMsg of Countries.Msg

    let init() =
        let summaryState, cmd = Summary.init()

        { countries = List.empty
          selected = None
          error = None
          currentUrl = Router.currentUrl()
          summaryState = summaryState
          countriesState = Countries.init() },
        Cmd.batch
            [ Cmd.ofMsg FetchCountries
              Cmd.map SummaryMsg cmd ]

    let update (msg: Msg) (state: State) =
        match msg with
        | SetSelected country ->
            let updateCountryInSummary = Cmd.ofMsg (Summary.Msg.SetCountry country)
            let updateCountryInCountries = Cmd.ofMsg (Countries.Msg.SetCountry country)
            { state with selected = country },
            Cmd.batch
                [ Cmd.none
                  Cmd.map SummaryMsg updateCountryInSummary
                  Cmd.map CountriesMsg updateCountryInCountries ]
        | SetCountries countries -> { state with countries = countries }, Cmd.none
        | FetchCountries ->
            let countriesP() =
                promise {
                    let! results = getCountries
                    return match results with
                           | Ok countries -> countries
                           | Error err ->
                               printfn "%A" err
                               failwith "Failed to fetch countreis"
                }
            state, Cmd.OfPromise.either countriesP () SetCountries FailedToFetch
        | FailedToFetch ex -> { state with error = Some ex.Message }, Cmd.none
        | UrlChanged url -> { state with currentUrl = url }, Cmd.none
        | SummaryMsg summaryMsg ->
            let summaryState, cmd = Summary.update summaryMsg state.summaryState
            { state with summaryState = summaryState }, Cmd.map SummaryMsg cmd
        | CountriesMsg countriesMsg ->
            let countriesState, cmd = Countries.update countriesMsg state.countriesState
            { state with countriesState = countriesState }, Cmd.map CountriesMsg cmd

    let navbar state dispatch =
        Html.ul
            [ prop.classes [ "app-navbar" ]
              prop.children
                  [ Html.li
                      [ prop.children
                          [ Html.a
                              [ prop.href (Router.format "summary")
                                prop.classes [ "app-navbar-item" ]
                                prop.text "Resumen" ] ] ]
                    Html.li
                        [ prop.children
                            [ Html.a
                                [ prop.href (Router.format "country")
                                  prop.classes [ "app-navbar-item" ]
                                  prop.text "Evolucion" ] ] ] ] ]

    let sidebar state dispatch =
        let selectedval =
            match state.selected with
            | Some selected -> selected.Slug
            | None -> ""
        Html.ul
            [ prop.classes [ "app-sidenav" ]
              prop.children
                  [ Html.header
                      [ Html.select
                          [ prop.valueOrDefault selectedval
                            prop.onChange (fun (country: string) ->
                                let found = state.countries |> List.tryFind (fun c -> c.Slug = country)
                                dispatch (SetSelected found))
                            prop.children
                                [ for country in state.countries do
                                    Html.option
                                        [ prop.value country.Slug
                                          prop.text country.Country ]
                                  Html.option
                                      [ prop.text "Seleccione un pais"
                                        prop.value "" ] ] ] ] ] ]



    let render (state: State) (dispatch: Msg -> unit) =
        let currentPage state dispatch =
            match state.currentUrl with
            | []
            | [ "summary" ] -> Summary.render state.summaryState (SummaryMsg >> dispatch)
            | [ "country" ] -> Countries.render state.countriesState (CountriesMsg >> dispatch)
            | [ "province" ] -> Html.h1 [ prop.text "Province" ]
            | _ -> Html.h1 [ prop.text "Not Found" ]

        let content state dispatch =
            Html.article
                [ prop.classes [ "app-content" ]
                  prop.children
                      [ currentPage state dispatch
                        Html.footer
                            [ prop.children
                                [ Html.span "Datos obtenidos de: "
                                  Html.a
                                      [ prop.target "_blank"
                                        prop.custom ("rel", "noopener noreferrer")
                                        prop.href "https://covid19api.com/"
                                        prop.text "https://covid19api.com/" ] ] ] ] ]

        let app =
            Html.main
                [ prop.classes [ "app-main" ]
                  prop.children
                      [ navbar state dispatch
                        sidebar state dispatch
                        content state dispatch ] ]

        Router.router
            [ Router.onUrlChanged (UrlChanged >> dispatch)
              Router.application app ]
