namespace Fable

module rec RemoteData =

    type RemoteData<'e, 'a> =
        | NotAsked
        | Loading
        | Failure of 'e
        | Success of 'a

    let map fn data =
        match data with
        | Success value -> Success (fn value)

        | Loading -> Loading

        | NotAsked -> NotAsked

        | Failure error -> Failure error

    let map2 fn a b =
        map fn a
            |> andMap b

    let mapError fn data : RemoteData<'f, 'a> =
        match data with
        | Success x -> Success x

        | Failure e -> Failure (fn e)

        | Loading -> Loading

        | NotAsked -> NotAsked

    let mapBoth successFn errorFn data : RemoteData<'f, 'b> =
        data
        |> map successFn
        |> mapError errorFn

    let andThen fn data : RemoteData<'e, 'b> =
        match data with
        | Success a -> fn a

        | Failure e -> Failure e

        | NotAsked -> NotAsked

        | Loading -> Loading

    let withDefault default_ (data : RemoteData<'e, 'a>) =
        match data with
        | Success x -> x

        | _ -> default_

    let unwrap default_ fn data : 'b =
        match data with
        | Success x -> fn x

        | _ -> default_

    let unpack defaultFn fn data : 'b =
        match data with
        | Success x -> fn x

        | _ -> defaultFn ()

    let fromOption error opt : RemoteData<'e, 'a> =
        match opt with
        | None -> Failure error

        | Some x -> Success x

    let fromResult result : RemoteData<'e, 'a> =
        match result with
        | Result.Error e -> Failure e

        | Ok x -> Success x

    let toOption (data : RemoteData<'e, 'a>) : Option<'a> =
        data
        |> map Some
        |> withDefault None

    let andMap wrappedValue wrappedFunction =
        match (wrappedFunction, wrappedValue) with
        | (Success fn, Success value) -> Success (fn value)

        | (Failure error, _) -> Failure error

        | (_, Failure error) -> Failure error

        | (Loading, _) -> Loading

        | (_, Loading) -> Loading

        | (NotAsked, _) -> NotAsked

        | (_, NotAsked) -> NotAsked

    let fromList list =
        List.foldBack (map2 (fun x -> List.append [x])) list (Success [])

    let succeed = Success

    let isSuccess data =
        match data with
        | Success _ -> true

        | _ -> false

    let isFailure data =
        match data with
        | Failure _ -> true

        | _ -> false

    let isLoading data =
        match data with
        | Loading -> true

        | _ -> false

    let isNotAsked data =
        match data with
        | NotAsked -> true

        | _ -> false