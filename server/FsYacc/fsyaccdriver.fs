module FsLexYacc.FsYacc.Driver

open System
open System.IO
open FSharp.Text.Lexing
open FsLexYacc.FsYacc
open FsLexYacc.FsYacc.AST
open Printf
open System.Collections.Generic
open TextUtils

let has_extension (s: string) =
    (s.Length >= 1 && s.[s.Length - 1] = '.') || Path.HasExtension(s)

let chop_extension (s: string) =
    if not (has_extension s) then
        invalidArg "s" "the file name does not have an extension"

    Path.Combine(Path.GetDirectoryName s, Path.GetFileNameWithoutExtension(s))

let checkSuffix (x: string) (y: string) = x.EndsWith(y)

let UnicodeStringAsLexbuf (filename, fileContent: string) : StringReader * LexBuffer<char> =
    let reader = new StringReader(fileContent)

    let lexbuf = LexBuffer.FromFunction(reader.Read)
    lexbuf.EndPos <- Position.FirstLine(filename)
    reader, lexbuf


let readSpecFromString fileName text =
    let reader, lexbuf = UnicodeStringAsLexbuf(fileName, text)
    use reader = reader

    try
        let spec = Parser.spec Lexer.token lexbuf
        Ok spec
    with e ->
        Result.Error(fileName, parserRangeToLspRange(lexbuf.StartPos, lexbuf.EndPos), e.Message)

let printTokensFromString filename text =
    let reader, lexbuf = UnicodeStringAsLexbuf(filename, text)
    use reader = reader

    try
        while true do
            printf "tokenize - getting one token"
            let t = Lexer.token lexbuf in
            printf "tokenize - got %s" (Parser.token_to_string t)

            if t = Parser.EOF then
                exit 0
    with e ->
        eprintf "%s(%d,%d): error: %s" filename lexbuf.StartPos.Line lexbuf.StartPos.Column e.Message
        exit 1

[<RequireQualifiedAccess; Struct; NoEquality; NoComparison>]
type SymbolKind =
    | Token
    | Type
    | Rule
    | RuleClauseRef
    | DollarRef of clauseRef: Identifier
    | Precedence
    | PrecedenceInRule

let specToSymbols (spec: ParserSpec) =
    let tokens = ResizeArray()

    for (token, type') in spec.Tokens do
        tokens.Add(token, SymbolKind.Token)
        if type'.IsSome then tokens.Add(type'.Value, SymbolKind.Type)
    for (token, type') in spec.Types do 
        tokens.Add(token, SymbolKind.Token)
        tokens.Add(type', SymbolKind.Type)

    spec.Associativities
    |> Seq.concat
    |> Seq.iter (fun (token, _) -> tokens.Add(token, SymbolKind.Precedence))

    for (rule, clauses, _) in spec.Rules do
        tokens.Add(rule, SymbolKind.Rule)
        for Rule(definitions, prec, code, _) in clauses do
            definitions
            |> List.iteri (fun i d ->
                tokens.Add(d, SymbolKind.RuleClauseRef)
                match code with
                | Some (code, r) ->
                    let posHelper = PosHelper(code, r.Start.Line)
                    System.Text.RegularExpressions.Regex.Matches(code, $@"(\${i + 1})(?=[^\d]|\s|$)")
                    |> Seq.iter (fun m ->
                        let c = m.Groups[1]
                        let r = posHelper.FindRange c.Value c.Index
                        tokens.Add((c.Value, r), SymbolKind.DollarRef d))
                | _ -> ()
            )

            match prec with
            | None -> ()
            | Some prec -> tokens.Add(prec, SymbolKind.PrecedenceInRule)

    tokens.ToArray()