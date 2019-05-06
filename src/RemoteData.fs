namespace Fable

module rec RemoteData =

    type RemoteData<'e, 'a> =
        | NotAsked
        | Loading
        | Failure of 'e
        | Success of 'a

    /// Maps the Success of a RemoteData
    let map fn data =
        match data with
        | Success value -> Success (fn value)

        | Loading -> Loading

        | NotAsked -> NotAsked

        | Failure error -> Failure error

    /// Combine two RemoteData sources with the given function.
    /// The result will be a Success only if both values are successes
    let map2 fn a b =
        map fn a
            |> andMap b

    /// Maps the Failure of a RemoteData
    let mapError fn data : RemoteData<'f, 'a> =
        match data with
        | Success x -> Success x

        | Failure e -> Failure (fn e)

        | Loading -> Loading

        | NotAsked -> NotAsked

    /// Maps the Failure and Success of a RemoteData
    let mapBoth successFn errorFn data : RemoteData<'f, 'b> =
        data
        |> map successFn
        |> mapError errorFn

    /// Chain RemoteData function calls
    let andThen fn data : RemoteData<'e, 'b> =
        match data with
        | Success a -> fn a

        | Failure e -> Failure e

        | NotAsked -> NotAsked

        | Loading -> Loading

    /// Returns the success of a RemoteData or some default value
    let withDefault default_ (data : RemoteData<'e, 'a>) =
        match data with
        | Success x -> x

        | _ -> default_

    /// Returns a default value if RemoteData is other than a Success.
    /// Otherwise returns the application of a given function to the value contained in Success
    let unwrap default_ fn data : 'b =
        match data with
        | Success x -> fn x

        | _ -> default_

    /// The same as `unwrap` but takes a function instead of a constant
    let unpack defaultFn fn data : 'b =
        match data with
        | Success x -> fn x

        | _ -> defaultFn ()

    /// Converts an `Option<'a>` to a `RemoteData<'e, 'a>` value
    let fromOption error opt : RemoteData<'e, 'a> =
        match opt with
        | None -> Failure error

        | Some x -> Success x

    /// Converts a `Result<'a, 'e>` to a `RemoteData<'e, 'a>` value
    let fromResult result : RemoteData<'e, 'a> =
        match result with
        | Result.Error e -> Failure e

        | Ok x -> Success x

    /// Converts a `RemoteData<'e, 'a>` to an `Option<'a>`
    let toOption (data : RemoteData<'e, 'a>) : Option<'a> =
        data
        |> map Some
        |> withDefault None

    /// Put the results of two RemoteData calls together.
    /// For example, if you were fetching three datasets, `a`, `b` and `c`,
    /// and wanted to end up with a tuple of all three, you could say:
    ///    let merge3 a b c =
    ///        map (fun a b c -> ( a, b, c )) a
    ///            |> andMap b
    ///            |> andMap c
    /// The final tuple succeeds only if all its children succeeded. It is
    /// still `Loading` if _any_ of its children are still `Loading`. And if
    /// any child fails, the error is the leftmost `Failure` value.
    let andMap wrappedValue wrappedFunction =
        match (wrappedFunction, wrappedValue) with
        | (Success fn, Success value) -> Success (fn value)

        | (Failure error, _) -> Failure error

        | (_, Failure error) -> Failure error

        | (Loading, _) -> Loading

        | (_, Loading) -> Loading

        | (NotAsked, _) -> NotAsked

        | (_, NotAsked) -> NotAsked

    /// Converts a list of RemoteData into a RemoteData of a list
    let fromList list =
        List.foldBack (map2 (fun x -> List.append [x])) list (Success [])

    /// Producess a RemoteData with a Success
    let succeed = Success

    /// Returns `true` if RemoteData is a Success
    let isSuccess data =
        match data with
        | Success _ -> true

        | _ -> false

    /// Returns `true` if RemoteData is a Failure
    let isFailure data =
        match data with
        | Failure _ -> true

        | _ -> false

    /// Returns `true` if RemoteData is Loading
    let isLoading data =
        match data with
        | Loading -> true

        | _ -> false

    /// Returns `true` if RemoteData is a NotAsked
    let isNotAsked data =
        match data with
        | NotAsked -> true

        | _ -> false