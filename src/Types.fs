namespace Fezcoro

module Types =
    open System

    type CovidStatus =
        | Confirmed
        | Recovered
        | Deaths

        member this.ToLower =
            match this with
            | Confirmed -> "confirmed"
            | Recovered -> "recovered"
            | Deaths -> "deaths"


    type Country =
        { Country: string
          Slug: string
          Provinces: string list }

    type CountryCase =
        { Country: string
          Province: string
          Lat: float
          Lon: float
          Date: DateTime
          Cases: int
          Status: string }

    type Summary =
        { Country: string
          NewConfirmed: int
          TotalConfirmed: int
          NewDeaths: int
          TotalDeaths: int
          NewRecovered: int
          TotalRecovered: int }
