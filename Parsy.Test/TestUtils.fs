﻿namespace Parsy.Test

open FsCheck
open Parsy

type TextParserType =
| ReferenceTextParser
| OptimisedTextParser

type ParserType =
| ReferenceParser
| OptimisedParser

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
                |> Seq.map (fun segment -> segment |> StringSegment.current, segment |> StringSegment.remaining)
                |> Set.ofSeq

    let makeParser =
        function
        | ReferenceParser ->
            fun parser input ->
                ReferenceParser.make parser input
                |> Set.ofList
        | OptimisedParser ->
            fun parser input ->
                let parses = ResizeArray ()
                OptimisedParser.make parser (fun a segment -> parses.Add (a, segment)) (StringSegment.ofString input)
                parses
                |> Seq.map (fun (a, segment) -> a, segment |> StringSegment.remaining)
                |> Set.ofSeq

    let config =
        { Config.QuickThrowOnFailure with
            Arbitrary =
                [
                typeof<TextParserGenerator.Marker>.DeclaringType
                typeof<ParserGenerator.Marker>.DeclaringType
                ]
            MaxTest = 10_000
            EndSize = 4
        }

    let check prop = Check.One(config, prop)
