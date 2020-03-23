namespace Fezcoro

module Endpoints =
    open System
    open Thoth.Fetch
    open Types
    open Fable.Core

    [<Literal>]
    let CovidApiUrl = "https://api.covid19api.com"


    let getCountries: JS.Promise<Result<Country list, FetchError>> =
        let url = CovidApiUrl + "/countries"
        promise { return! Fetch.tryGet (url) }

    let getSummary: JS.Promise<Result<{| Countries: Summary list; Date: DateTime |}, FetchError>> =
        let url = CovidApiUrl + "/summary"
        promise { return! Fetch.tryGet (url) }

    let getCasesByCountryByStatus
        (country: string)
        (status: CovidStatus)
        : JS.Promise<Result<CountryCase list, FetchError>> =
        let endpoint = CovidApiUrl + sprintf "/country/%s/status/%s" country status.ToLower
        promise { return! Fetch.tryGet (endpoint) }

    let getTotalCasesByCountryByStatus
        (country: string)
        (status: CovidStatus)
        : JS.Promise<Result<CountryCase list, FetchError>>
        =
        let endpoint = CovidApiUrl + sprintf "/total/country/%s/status/%s" country status.ToLower
        promise { return! Fetch.tryGet (endpoint) }

    let getCasesByCountryByStatusDayOne
        (country: string)
        (status: CovidStatus)
        : JS.Promise<Result<CountryCase list, FetchError>>
        =
        let endpoint = CovidApiUrl + sprintf "/dayone/country/%s/status/%s" country status.ToLower
        promise { return! Fetch.tryGet (endpoint) }

    let getTotalCasesByCountryByStatusDayOne
        (country: string)
        (status: CovidStatus)
        : JS.Promise<Result<CountryCase list, FetchError>>
        =
        let endpoint = CovidApiUrl + sprintf "/total/dayone/country/%s/status/%s" country status.ToLower
        promise { return! Fetch.tryGet (endpoint) }
