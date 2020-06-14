namespace Parsy.Test

open FsCheck
open Parsy

type TextParserType =
| ReferenceTextParser
| OptimisedTextParser

[<RequireQualifiedAccess>]
module TestUtils =

    let makeTextParser =
        function
        | ReferenceTextParser ->
            fun parser input ->
                ReferenceTextParser.make parser input
                |> Set.ofList
        | OptimisedTextParser ->
            fun parser input ->
                let parses = ResizeArray ()
                OptimisedTextParser.make parser parses.Add (StringSegment.ofString input)
                parses
                |> Seq.map (fun segment -> segment |> StringSegment.consumed, segment |> StringSegment.remaining)
                |> Set.ofSeq

    let config =
        { Config.QuickThrowOnFailure with
            Arbitrary = [ typeof<TextParserGenerator.Marker>.DeclaringType ]
            MaxTest = 10_000
            EndSize = 4
        }

    let check prop = Check.One(config, prop)
