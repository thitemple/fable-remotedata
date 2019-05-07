# Fable.RemoteData [![Build Status](https://travis-ci.org/thitemple/fable-remotedata.svg?branch=master)](https://travis-ci.org/thitemple/fable-remotedata) ![Nuget](https://img.shields.io/nuget/v/Fable.RemoteData.svg)

F# implementation for Fable of [Elm's RemoteData package](https://github.com/krisajenkins/remotedata).

## How to use it

Here's a sample using Elmish with Fetch

```
open Fable.RemoteData
open Fetch

// Define your types

type Film = {
    title: string
}

type Model = {
    Films: WebData<Film list>
}

type Msg =
    | HandleFilmsFetched of WebData<Film list>

// Handle the State

let init () =
    { Films = Loading }, Cmd.ofPromise.perform (fetch "https://swapi.co/api/films") [] (fun res -> // handle the response and produce a Msg)

let update msg model =
    match msg with
    | HandleFilmsFetched data -> { model with Films = data }

// In your views

let root model dispatch =
    match model.Films with
    | Succeed films ->
        div [] [ List.length films |> toString |> str ]

    | Failure err ->
        div [] [ str err ]

    | Loading ->
        div [] [ str "Still loading your data" ]

    | NotAsked ->
        div [] [ str "You have yet to fetch some data" ]
```

## How to run it locally

- `npm i`
- Run tests: `npm test`
- Publish: `npm run build Publish`
