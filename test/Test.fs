module Test

open Fable.Core
open Fable.RemoteData

let inline equal (expected : 'T) (actual : 'T) : unit =
    Testing.Assert.AreEqual(expected, actual)

[<Global>]
let describe (msg : string) (f : unit -> unit) : unit = jsNative

[<Global>]
let it (msg : string) (f : unit -> unit) : unit = jsNative

describe "RemoteData tests" <| fun () ->
    describe "map" <| fun () ->
        it "returns NotAsked when data was NotAsked" <| fun () ->
            map int NotAsked
            |> equal NotAsked

        it "returns Loading when data was Loading" <| fun () ->
            map int Loading
            |> equal Loading
        
        it "returns Failure when data was Failure" <| fun () ->
            map int (Failure "")
            |> equal (Failure "")
        
        it "maps the data when it was a Success" <| fun () ->
            map int (Success "42")
            |> equal (Success 42)

    describe "mapError" <| fun () ->
        it "return NotAsked when data was NotAsked" <| fun () ->
            mapError int NotAsked
            |> equal NotAsked

        it "return Loading when data was Loading" <| fun () ->
            mapError int Loading
            |> equal Loading

        it "return Success when data was Success" <| fun () ->
            mapError int (Success "42")
            |> equal (Success "42")
        
        it "maps the error when data is a Failure" <| fun () ->
            mapError int (Failure "42")
            |> equal (Failure 42)

    describe "mapBoth" <| fun () ->
        it "maps a Success" <| fun () ->
            mapBoth int int (Success "42")
            |> equal (Success 42)

        it "maps a Failure" <| fun () ->
            mapBoth int int (Failure "42")
            |> equal (Failure 42)

    describe "withDefault" <| fun () ->
        it "returns the data from a Success" <| fun () ->
            withDefault 13 (Success 42)
            |> equal 42
        
        it "returns the default value when it's a Failure" <| fun () ->
            withDefault 13 (Failure 42)
            |> equal 13
        
        it "returns the default value when it's Loading" <| fun () ->
            withDefault 13 Loading
            |> equal 13
        
        it "returns the default value when it's NotAsked" <| fun () ->
            withDefault 13 NotAsked
            |> equal 13

    describe "unwrap" <| fun () ->
        it "applyes the function when Success" <| fun () ->
            unwrap 13 ((+) 1) (Success 41)
            |> equal 42
        
        it "returns the default value when Failure" <| fun () ->
            unwrap 13 ((+) 1) (Failure "error")
            |> equal 13
        
        it "returns the default value when Loading" <| fun () ->
            unwrap 13 ((+) 1) Loading
            |> equal 13
        
        it "returns the default value when NotAsked" <| fun () ->
            unwrap 13 ((+) 1) NotAsked
            |> equal 13

    describe "unpack" <| fun () ->
        it "applies the function when Success" <| fun () ->
            unpack (fun () -> 13) ((+) 1) (Success 41)
            |> equal 42
        
        it "returns the default when Failure" <| fun () ->
            unpack (fun () -> 13) ((+) 1) (Failure "error")
            |> equal 13
        
        it "returns the default when Loading" <| fun () ->
            unpack (fun () -> 13) ((+) 1) Loading
            |> equal 13
        
        it "returns the default when NotAsked" <| fun () ->
            unpack (fun () -> 13) ((+) 1) NotAsked
            |> equal 13

    describe "fromOption" <| fun () ->
        it "returns Success from a Some value" <| fun () ->
            fromOption "error" (Some 42)
            |> equal (Success 42)
        
        it "returns Failure from a None value" <| fun () ->
            fromOption "error" None
            |> equal (Failure "error")

    describe "fromResult" <| fun () ->
        it "returns Success from an Ok value" <| fun () ->
            fromResult (Ok 42)
            |> equal (Success 42)
        
        it "returns Failure from an Error value" <| fun () ->
            fromResult (Error "error")
            |> equal (Failure "error")

    describe "toOption" <| fun () ->
        it "returns a Some from a Success value" <| fun () ->
            toOption (Success 42)
            |> equal (Some 42)
        
        it "returns a None from a Failure value" <| fun () ->
            toOption (Failure "error")
            |> equal None
        
        it "returns a None from a Loading value" <| fun () ->
            toOption Loading
            |> equal None
        
        it "returns a None from a NotAsked value" <| fun () ->
            toOption NotAsked
            |> equal None

    describe "andMap" <| fun () ->
        it "maps two successes" <| fun () ->
            andMap (Success 21) (Success ((*) 2))
            |> equal (Success 42)
        
        it "maps Failure on the first case" <| fun () ->
            andMap (Failure "error") Loading
            |> equal (Failure "error")
        
        it "maps Failure on the second case" <| fun () ->
            andMap Loading (Failure "error")
            |> equal (Failure "error")

    describe "fromList" <| fun () ->
        it "creates from an empty list" <| fun () ->
            fromList []
            |> equal (Success [])
        
        it "Success with many values" <| fun () ->
            fromList [ Success 1; Success 2 ]
            |> equal ( Success [1 ; 2] )
        
        it "returns Loading when there's no Failure" <| fun () ->
            fromList [ Loading; NotAsked ]
            |> equal ( Loading )
        
        it "returns Loading even with a Success" <| fun () ->
            fromList [ Loading; Success 1 ]
            |> equal ( Loading )
        
        it "returns Failure when there's one " <| fun () ->
            fromList [ Loading; Success 1; Failure "error" ]
            |> equal ( Failure "error" )

        it "returns first Failure when tehre's more than one" <| fun () ->
            fromList [ Loading; Success 1; Failure "error"; Failure "fail" ]
            |> equal ( Failure "error" )


    it "returns a Success from succeed" <| fun () ->
        succeed 42
        |> equal (Success 42)
    
    it "returns true from isSuccess from a Success" <| fun () ->
        isSuccess (Success 42)
        |> equal true
    
    it "returns true from isFailure from a Failure" <| fun () ->
        isFailure (Failure "error")
        |> equal true
    
    it "returns true from isLoading from a Loading" <| fun () ->
        isLoading Loading
        |> equal true
    
    it "returns true from isNotAsked from a NotAsked" <| fun () ->
        isNotAsked NotAsked
        |> equal true
