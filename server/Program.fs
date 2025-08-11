open Ionide.LanguageServerProtocol
open StreamJsonRpc
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server
open System.Collections.Generic
open FsToolkit.ErrorHandling
open System.IO
open System
open TextUtils
open Serilog.Core
open Serilog
open System.IO

type FsYaccSymbolKind = FsLexYacc.FsYacc.Driver.SymbolKind
type ParserSpec = FsLexYacc.FsYacc.AST.ParserSpec

[<Struct; RequireQualifiedAccess>]
type CompletionContext =
    | Invalid
    | RuleClause
    | RuleClauseAfterPrecedence
    | RuleClauseCode of (string * string) array

// type Config =
//     { ShowDollarReferenceInFindingReferencesOfRulesOrTokens: bool }

//     static member Default = { ShowDollarReferenceInFindingReferencesOfRulesOrTokens = false }

let makeCallHierarchyItem name token uri r =
    { CallHierarchyItem.Data = Some(Json.fromObject token)
      Name = name
      Kind = SymbolKind.Function
      Tags = None
      Detail = None
      Uri = uri
      Range = r
      SelectionRange = r }

let buildDescription (spec: ParserSpec) theToken kind =
    match kind with
    | FsYaccSymbolKind.Type -> Some $"type {theToken}"
    | FsYaccSymbolKind.Token ->
        let ty =
            spec.Tokens
            |> List.tryFind (fun ((x, _), _) -> x = theToken)
            |> Option.bind snd
            |> Option.orElseWith (fun () ->
                spec.Types |> List.tryFind (fun ((x, _), _) -> x = theToken) |> Option.map snd)

        match ty with
        | None -> Some $"%%token {theToken}"
        | Some(ty, _) -> Some $"%%token <{ty}> {theToken}"
    | FsYaccSymbolKind.Rule ->
        spec.Rules
        |> List.tryFind (fun ((x, _), _, _) -> x = theToken)
        |> Option.map (fun (_, clauses, _) ->
            seq {
                $"{theToken}: "

                for FsLexYacc.FsYacc.AST.Rule(definitions = definitions) in clauses do
                    let definitions = definitions |> List.map fst
                    let definitions = String.concat " " definitions
                    $"  | {definitions}"
            }
            |> String.concat "\n")
    | FsYaccSymbolKind.Precedence
    | FsYaccSymbolKind.PrecedenceInRule ->
        spec.Associativities
        |> List.tryPick (List.tryFind (fun ((x, _), _) -> x = theToken))
        |> Option.map (fun (_, associativity) ->
            let associativity =
                match associativity with
                | FsLexYacc.FsYacc.AST.Associativity.LeftAssoc -> "left"
                | FsLexYacc.FsYacc.AST.Associativity.RightAssoc -> "right"
                | FsLexYacc.FsYacc.AST.Associativity.NonAssoc -> "nonassoc"

            $"%%{associativity} {theToken}")
    | FsYaccSymbolKind.RuleClauseRef
    | FsYaccSymbolKind.DollarRef _ -> None

type YaccLspServer(client: LspClient, logger: Logger) =
    inherit LspServer()

    let log (msg: string) =
#if DEBUG
        logger.Information($"[{DateTime.Now}] {msg}")
#else
        ()
#endif

    let notPossible (param: 't) =
        logger.Error("Entered a not possible path.\n{}", [| param |])
        failwith "Entered a not possible path."

    let docs = System.Collections.Concurrent.ConcurrentDictionary()
    let parseResults = System.Collections.Concurrent.ConcurrentDictionary()

    let getFileName uri =
        let path = Uri(uri).LocalPath.[1..]
        Path.GetFileName(path)

    let loadFile uri =
        match docs.TryGetValue uri with
        | true, (file: string) -> Some file
        | false, _ -> None

    let getLine uri column =
        match docs.TryGetValue uri with
        | true, (file: string) ->
            let column = int column
            let arr = file.Split '\n'
            if arr.Length >= column then Some arr[column] else None
        | false, _ -> None

    let mkDiagnostic range message =
        { Diagnostic.Code = None
          Message = message
          Range = range
          Severity = Some DiagnosticSeverity.Error
          Source = Some "FsYacc"
          RelatedInformation = None
          Tags = None
          Data = None
          CodeDescription = None }

    let checkMissingCodeBlock (spec: ParserSpec) =
        spec.Rules
        |> Seq.collect (fun (_, clauses, ruleRange) ->
            let _, n =
                ((ruleRange.Start, None), clauses)
                ||> List.fold (fun (endPos, state as r) x ->
                    if state.IsNone then
                        match x with
                        | FsLexYacc.FsYacc.AST.Rule(code = None; range = range) ->
                            range.End,
                            mkDiagnostic { Start = endPos; End = range.End } "This rule clause has no code block."
                            |> Some
                        | FsLexYacc.FsYacc.AST.Rule(range = range) -> range.End, None
                    else
                        r)

            match n with
            | Some n -> [ n ]
            | None -> [])

    let parseFileWithDiagnostic uri buildDiagnostic =
        let diagnostics = if buildDiagnostic then ResizeArray() else null

        let checkMissingCodeBlock spec =
            if buildDiagnostic then
                diagnostics.AddRange(checkMissingCodeBlock spec)

        let diagnosticsArray () =
            if isNull diagnostics then
                Array.empty
            else
                diagnostics.ToArray()

        async {
            try
                let filename = getFileName uri

                match loadFile uri with
                | Some file ->
                    let hash' = hash file

                    match parseResults.TryGetValue uri with
                    | true, (hash'', spec, tokens) when hash' = hash'' ->
                        checkMissingCodeBlock spec
                        return spec, tokens, diagnosticsArray ()
                    | _ ->
                        match FsLexYacc.FsYacc.Driver.readSpecFromString filename file with
                        | Ok spec ->
                            let symbols = FsLexYacc.FsYacc.Driver.specToSymbols spec
                            parseResults.[uri] <- (hash', spec, symbols)
                            checkMissingCodeBlock spec
                            return spec, symbols, diagnosticsArray ()
                        | Error(_, range, err) ->
                            if buildDiagnostic then
                                diagnostics.Add(mkDiagnostic range err)

                            match parseResults.TryGetValue uri with
                            | true, (_, spec, tokens) ->
                                checkMissingCodeBlock spec
                                return spec, tokens, diagnosticsArray ()
                            | _ -> return ParserSpec.Empty, Array.empty, diagnosticsArray ()
                | None -> return ParserSpec.Empty, Array.empty, Array.empty
            with ex ->
                log $"parseFile {uri} \nerr: {ex}"

                if buildDiagnostic then
                    mkDiagnostic
                        { Start = { Line = 0u; Character = 1u }
                          End = { Line = 1000u; Character = 1u } }
                        ex.Message
                    |> diagnostics.Add

                return ParserSpec.Empty, Array.empty, diagnosticsArray ()
        }

    let parseFile uri =
        asyncResult {
            let! spec, tokens, diagnostics = parseFileWithDiagnostic uri false
            return spec, tokens
        }
        |> AsyncResult.mapError JsonRpc.Error.InternalErrorMessage

    override _.Dispose() : unit = logger.Dispose()

    override _.Initialize(p) =
        log $"Initialize {p.RootUri}"

        { InitializeResult.Default with
            Capabilities =
                { ServerCapabilities.Default with
                    DefinitionProvider = Some(U2.C1 true)
                    ReferencesProvider = Some(U2.C1 true)
                    HoverProvider = Some(U2.C1 true)
                    CallHierarchyProvider = Some(U3.C1 true)
                    DocumentSymbolProvider =
                        Some
                        <| U2.C2
                            { Label = Some "FsYacc"
                              WorkDoneProgress = Some false }

                    InlayHintProvider =
                        Some
                        <| U3.C2
                            { ResolveProvider = Some false
                              WorkDoneProgress = Some false }
                    CompletionProvider =
                        Some
                            { ResolveProvider = Some false
                              TriggerCharacters = Some([| "."; "'"; "$" |])
                              AllCommitCharacters = None
                              CompletionItem = None
                              WorkDoneProgress = Some false }
                    TextDocumentSync =
                        Some(
                            U2.C1
                                { Types.TextDocumentSyncOptions.Default with
                                    Change = Some TextDocumentSyncKind.Full
                                    OpenClose = Some true }
                        )
                    DiagnosticProvider =
                        Some(
                            U2.C1
                                { Types.DiagnosticOptions.WorkDoneProgress = Some false
                                  Identifier = Some "FsYacc"
                                  InterFileDependencies = false
                                  WorkspaceDiagnostics = false }
                        ) } }
        |> Ok
        |> Async.retn

    override _.TextDocumentDefinition(arg: TextDocumentPositionParams) : AsyncLspResult<Definition option> =
        asyncResultOption {
            log $"TextDocumentDefinition {arg.TextDocument.Uri} {arg.Position}"
            let uri = arg.TextDocument.Uri
            let pos = arg.Position
            let! spec, tokens = parseFile uri

            let! ((theToken, _), kind) = tokens |> Array.tryFind (fun ((_, r), _) -> posInRange pos r)

            log $"TextDocumentDefinition theToken: {theToken}, kind: {kind}"

            match kind with
            | FsYaccSymbolKind.Token
            | FsYaccSymbolKind.Type
            | FsYaccSymbolKind.Rule
            | FsYaccSymbolKind.Precedence ->
                log $"TextDocumentDefinition skip this kind '{kind}'"
                return U2.C2 Array.empty
            | FsYaccSymbolKind.RuleClauseRef ->
                let ranges =
                    tokens
                    |> Array.choose (fun ((x, r), k) ->
                        if (k.IsToken || k.IsRule) && x = theToken then
                            Some({ Uri = uri; Range = r })
                        else
                            None)

                log $"TextDocumentDefinition ranges: {ranges.Length}"
                return U2.C2 ranges
            | FsYaccSymbolKind.DollarRef(_, r) -> return U2.C1 { Uri = uri; Range = r }
            | FsYaccSymbolKind.PrecedenceInRule ->
                return!
                    spec.Associativities
                    |> List.tryPick (
                        List.tryPick (fun ((x, r), _) ->
                            if x = theToken then
                                Some(U2.C1 { Uri = uri; Range = r })
                            else
                                None)
                    )
        }

    override _.TextDocumentReferences(arg: ReferenceParams) : AsyncLspResult<Location array option> =
        asyncResultOption {
            log $"TextDocumentReferences {arg.TextDocument.Uri} {arg.Position}"
            let uri = arg.TextDocument.Uri
            let pos = arg.Position
            let! spec, tokens = parseFile uri

            let! ((theToken, r), kind) = tokens |> Array.tryFind (fun ((_, r), _) -> posInRange pos r)

            let! ((theToken, _), kind) =
                if kind.IsRuleClauseRef then
                    tokens
                    |> Array.tryFind (fun ((i, r), k) -> (k.IsToken || k.IsRule) && i = theToken)
                else
                    Some((theToken, r), kind)

            log $"TextDocumentReferences theToken: {theToken}, kind: {kind}"

            let pickTokens predicator =
                tokens
                |> Array.choose (fun ((x, r), k) ->
                    if predicator x r k then
                        Some({ Uri = uri; Range = r })
                    else
                        None)


            match kind with
            | FsYaccSymbolKind.Token ->
                let ranges =
                    pickTokens (fun x r k -> x = theToken && (k.IsRule || k.IsRuleClauseRef))

                log $"TextDocumentReferences ranges: {ranges.Length}"
                return ranges

            | FsYaccSymbolKind.Type ->
                let ranges = pickTokens (fun x r k -> x = theToken && k.IsType)
                log $"TextDocumentReferences ranges: {ranges.Length}"
                return ranges

            | FsYaccSymbolKind.Rule ->
                let ranges = pickTokens (fun x r k -> x = theToken && k.IsRuleClauseRef)
                log $"TextDocumentReferences ranges: {ranges.Length}"
                return ranges

            | FsYaccSymbolKind.RuleClauseRef ->
                log $"TextDocumentReferences RuleClauseRef unpossible"
                return Array.empty

            | FsYaccSymbolKind.DollarRef t ->
                let ranges =
                    tokens
                    |> Array.choose (fun ((x, r), k) ->
                        match k with
                        | FsYaccSymbolKind.DollarRef u when t = u -> Some({ Uri = uri; Range = r })
                        | _ -> None)

                log $"TextDocumentReferences ranges: {ranges.Length}"
                return ranges

            | FsYaccSymbolKind.Precedence
            | FsYaccSymbolKind.PrecedenceInRule ->
                let ranges = pickTokens (fun x r k -> x = theToken && k.IsPrecedenceInRule)
                log $"TextDocumentReferences ranges: {ranges.Length}"
                return ranges
        }

    override _.TextDocumentHover(arg: TextDocumentPositionParams) : AsyncLspResult<Hover option> =
        asyncResultOption {
            log $"TextDocumentHover {arg.TextDocument.Uri} {arg.Position}"
            let uri = arg.TextDocument.Uri
            let pos = arg.Position
            let! spec, tokens = parseFile uri

            let! ((theToken, r), kind) = tokens |> Array.tryFind (fun ((_, r), _) -> posInRange pos r)

            let! ((theToken, r), kind) =
                match kind with
                | FsYaccSymbolKind.DollarRef(x, _) ->
                    tokens |> Array.tryFind (fun ((i, r), k) -> (k.IsToken || k.IsRule) && i = x)
                | FsYaccSymbolKind.RuleClauseRef ->
                    tokens
                    |> Array.tryFind (fun ((i, r), k) -> (k.IsToken || k.IsRule) && i = theToken)
                | _ -> Some((theToken, r), kind)

            let! description = buildDescription spec theToken kind

            let r =
                { Contents =
                    U3.C3
                        [| U2.C2
                               { Language = "FsYacc"
                                 Value = description } |]
                  Range = None }

            log $"TextDocumentHover ({r})"
            return r
        }

    override _.TextDocumentDocumentSymbol
        (arg: DocumentSymbolParams)
        : AsyncLspResult<U2<SymbolInformation array, DocumentSymbol array> option> =
        asyncResultOption {
            log $"TextDocumentDocumentSymbol {arg.TextDocument.Uri}"
            let uri = arg.TextDocument.Uri
            let! spec, tokens = parseFile uri

            let r =
                tokens
                |> Array.choose (fun ((x, r), k) ->
                    match k with
                    | FsYaccSymbolKind.Type
                    | FsYaccSymbolKind.Token
                    | FsYaccSymbolKind.DollarRef _
                    | FsYaccSymbolKind.RuleClauseRef
                    | FsYaccSymbolKind.Precedence
                    | FsYaccSymbolKind.PrecedenceInRule -> None
                    | FsYaccSymbolKind.Rule ->
                        { Kind = SymbolKind.Function
                          Deprecated = None
                          ContainerName = None
                          Location =
                            { Uri = arg.TextDocument.Uri
                              Range = r }
                          Name = x
                          Tags = None }
                        |> Some)

            return U2.C1 r
        }

    override _.TextDocumentInlayHint(arg: InlayHintParams) : AsyncLspResult<InlayHint array option> =
        asyncResultOption {
            log $"TextDocumentInlayHint {arg.TextDocument.Uri}"
            let uri = arg.TextDocument.Uri
            let! spec, tokens = parseFile uri

            let r =
                tokens
                |> Array.choose (fun ((_, r), k) ->
                    match k with
                    | FsYaccSymbolKind.DollarRef(name, _) ->
                        Some
                            { Position = r.End
                              Label = U2.C1 name
                              Tooltip = None
                              Kind = None
                              TextEdits = None
                              Data = None
                              PaddingLeft = Some true
                              PaddingRight = None }
                    | _ -> None)

            return r
        }

    override _.TextDocumentCompletion(arg: CompletionParams) : AsyncLspResult<CompletionList option> =
        let makeCompletion (spec: ParserSpec) (x: string) k withName =
            let name = defaultArg withName x
            let label = if withName.IsSome then $"{name} ({x})" else name

            let completionKind =
                match k with
                | FsYaccSymbolKind.Token -> Some CompletionItemKind.Value
                | FsYaccSymbolKind.Rule -> Some CompletionItemKind.Function
                | FsYaccSymbolKind.Precedence -> Some CompletionItemKind.Constant
                | _ -> None

            buildDescription spec x k
            |> Option.map (fun description ->
                { CompletionItem.Create(label) with
                    Kind = completionKind
                    InsertText = Some name
                    Detail = Some description })

        let makeKeywordCompletion name =
            { CompletionItem.Create(name) with
                Kind = Some CompletionItemKind.Keyword }

        let findCompletions ctx spec (tokens: (_ * FsYaccSymbolKind) array) =
            let r =
                match ctx with
                | CompletionContext.Invalid ->
                    seq {
                        "%token"
                        "%type"
                        "%start"
                        "%left"
                        "%right"
                        "%nonassoc"
                    }
                    |> Seq.map makeKeywordCompletion
                    |> Array.ofSeq
                | CompletionContext.RuleClauseAfterPrecedence ->
                    tokens
                    |> Array.choose (fun ((x, r), k) ->
                        if k.IsPrecedence then
                            makeCompletion spec x k None
                        else
                            None)
                | CompletionContext.RuleClause ->
                    tokens
                    |> Array.choose (fun ((x, r), k) ->
                        if k.IsRule || k.IsToken then
                            makeCompletion spec x k None
                        else
                            None)
                    |> Array.insertAt 0 (makeKeywordCompletion "%prec")
                | CompletionContext.RuleClauseCode arr ->
                    let tokens =
                        let oldnames =
                            match ctx with
                            | CompletionContext.RuleClauseCode arr ->
                                let a, b = arr |> Array.unzip
                                HashSet b
                            | _ -> HashSet()

                        tokens
                        |> Array.choose (fun ((x, r), k) ->
                            if oldnames.Contains(x) && k.IsToken || k.IsRule then
                                Some(x, k)
                            else
                                None)
                        |> Map.ofArray

                    arr
                    |> Array.choose (fun (name, referencedName) ->
                        match tokens.TryFind(referencedName) with
                        | None -> None
                        | Some k -> makeCompletion spec referencedName k (Some name))
                |> Array.distinctBy (fun x -> x.Label)

            { IsIncomplete = false
              Items = r
              ItemDefaults = None }

        asyncResultOption {
            log $"TextDocumentCompletion {arg.TextDocument.Uri}"
            let uri = arg.TextDocument.Uri
            let! spec, tokens = parseFile uri

            let definitionsOpt =
                spec.Rules
                |> List.tryPick (fun (_, ls, range) ->
                    if posIsAfter arg.Position range.Start then
                        ls
                        |> List.tryPick (fun (FsLexYacc.FsYacc.AST.Rule(definitions, _, code, range)) ->
                            if posIsBefore arg.Position range.End then
                                match code with
                                | Some(_, codeRange) -> Some(definitions, Some codeRange)
                                | None -> Some(definitions, None)
                            else
                                None)
                    else
                        None)

            match definitionsOpt with
            | None -> return findCompletions CompletionContext.Invalid spec tokens
            | Some(definitions, Some codeRange) when posInRangeWithoutBoundary arg.Position codeRange ->
                let ctx =
                    definitions
                    |> List.mapi (fun i (x, _) -> $"${i + 1}", x)
                    |> List.toArray
                    |> CompletionContext.RuleClauseCode

                return findCompletions ctx spec tokens
            | Some _ ->
                let! line = getLine uri arg.Position.Line

                let ctx =
                    let pos = int64 (line.IndexOf "%prec")

                    if -1L < pos && pos < int64 arg.Position.Character then
                        CompletionContext.RuleClauseAfterPrecedence
                    else
                        CompletionContext.RuleClause

                return findCompletions ctx spec tokens
        }

    override _.TextDocumentPrepareCallHierarchy
        (arg: CallHierarchyPrepareParams)
        : AsyncLspResult<CallHierarchyItem array option> =
        asyncResultOption {
            log $"TextDocumentPrepareCallHierarchy {arg.TextDocument.Uri}"
            let uri = arg.TextDocument.Uri
            let pos = arg.Position
            let! spec, tokens = parseFile uri
            let! ((theToken, r), kind) = tokens |> Array.tryFind (fun ((_, r), _) -> posInRange pos r)

            let! ((theToken, r), kind) =
                match kind with
                | FsYaccSymbolKind.DollarRef(x, _) ->
                    tokens |> Array.tryFind (fun ((i, r), k) -> (k.IsToken || k.IsRule) && i = x)
                | FsYaccSymbolKind.RuleClauseRef ->
                    tokens
                    |> Array.tryFind (fun ((i, r), k) -> (k.IsToken || k.IsRule) && i = theToken)
                | _ -> Some((theToken, r), kind)

            match kind with
            | FsYaccSymbolKind.Token
            | FsYaccSymbolKind.Rule -> return [| makeCallHierarchyItem theToken (theToken, r) uri r |]
            | FsYaccSymbolKind.RuleClauseRef -> return! None
            | FsYaccSymbolKind.Type
            | FsYaccSymbolKind.DollarRef _
            | FsYaccSymbolKind.Precedence
            | FsYaccSymbolKind.PrecedenceInRule -> return! None
        }

    override _.CallHierarchyIncomingCalls
        (arg: CallHierarchyIncomingCallsParams)
        : AsyncLspResult<CallHierarchyIncomingCall array option> =
        asyncResultOption {
            log $"CallHierarchyIncomingCalls {arg.Item.Name}"
            let uri = arg.Item.Uri
            // let pos = arg.Item.Range.Start
            let! spec, tokens = parseFile uri

            let! data = arg.Item.Data
            let (lastToken, lastR) = data.ToObject<string * LspRange>()
            let! ((theToken, r), kind) = tokens |> Array.tryFind (fun ((_, r), _) -> r = lastR)

            let! ((theToken, r), kind) =
                match kind with
                | FsYaccSymbolKind.DollarRef(x, _) ->
                    tokens |> Array.tryFind (fun ((i, r), k) -> (k.IsToken || k.IsRule) && i = x)
                | FsYaccSymbolKind.RuleClauseRef ->
                    tokens
                    |> Array.tryFind (fun ((i, r), k) -> (k.IsToken || k.IsRule) && i = theToken)
                | _ -> Some((theToken, r), kind)

            match kind with
            | FsYaccSymbolKind.Token
            | FsYaccSymbolKind.Rule ->
                let r =
                    spec.Rules
                    |> List.collect (fun ((ruleName, ruleRange), j, _) ->
                        j
                        |> List.indexed
                        |> List.choose (fun (i, (FsLexYacc.FsYacc.AST.Rule(definitions = definitions))) ->
                            if definitions.IsEmpty then
                                None
                            else
                                let ranges =
                                    definitions
                                    |> List.choose (fun (x, r) -> if x = theToken then Some r else None)
                                    |> Array.ofList

                                let rr = definitions.Head |> snd

                                match ranges with
                                | [||] -> None
                                | _ ->
                                    Some
                                        { CallHierarchyIncomingCall.From =
                                            makeCallHierarchyItem $"{ruleName} #{i + 1}" (ruleName, ruleRange) uri rr
                                          FromRanges = ranges }))

                return r |> Array.ofList
            | FsYaccSymbolKind.RuleClauseRef -> return! None
            | FsYaccSymbolKind.Type
            | FsYaccSymbolKind.DollarRef _
            | FsYaccSymbolKind.Precedence
            | FsYaccSymbolKind.PrecedenceInRule -> return! None

        }

    override _.CallHierarchyOutgoingCalls
        (arg: CallHierarchyOutgoingCallsParams)
        : AsyncLspResult<CallHierarchyOutgoingCall array option> =
        asyncResultOption {
            log $"CallHierarchyOutgoingCalls {arg.Item.Name}"
            let uri = arg.Item.Uri
            // let pos = arg.Item.Range.Start
            let! spec, tokens = parseFile uri

            let! data = arg.Item.Data
            let (lastToken, lastR) = data.ToObject<string * LspRange>()
            let! ((theToken, r), kind) = tokens |> Array.tryFind (fun ((_, r), _) -> r = lastR)

            let! ((theToken, r), kind) =
                match kind with
                | FsYaccSymbolKind.DollarRef(x, _) ->
                    tokens |> Array.tryFind (fun ((i, r), k) -> (k.IsToken || k.IsRule) && i = x)
                | FsYaccSymbolKind.RuleClauseRef ->
                    tokens
                    |> Array.tryFind (fun ((i, r), k) -> (k.IsToken || k.IsRule) && i = theToken)
                | _ -> Some((theToken, r), kind)

            match kind with
            | FsYaccSymbolKind.Rule ->
                let r =
                    spec.Rules
                    |> List.filter (fun ((x, _), _, _) -> x = theToken)
                    |> List.collect (fun ((ruleName, ruleRange), j, _) ->
                        j
                        |> List.collect (fun (FsLexYacc.FsYacc.AST.Rule(definitions = definitions)) -> definitions)
                        |> List.groupBy fst
                        |> List.map (fun (x, xs) ->
                            let ranges = xs |> List.toArray |> Array.map snd

                            { CallHierarchyOutgoingCall.To = makeCallHierarchyItem x (x, ranges[0]) uri ruleRange
                              FromRanges = ranges }))

                return r |> Array.ofList
            | FsYaccSymbolKind.Token
            | FsYaccSymbolKind.RuleClauseRef
            | FsYaccSymbolKind.Type
            | FsYaccSymbolKind.DollarRef _
            | FsYaccSymbolKind.Precedence
            | FsYaccSymbolKind.PrecedenceInRule -> return! None
        }

    override _.TextDocumentDidOpen(arg: DidOpenTextDocumentParams) : Async<unit> =
        async {
            log $"TextDocumentDidOpen {arg.TextDocument.Uri}"

            docs.AddOrUpdate(arg.TextDocument.Uri, arg.TextDocument.Text, (fun _ _ -> arg.TextDocument.Text))
            |> ignore

            let! _ = parseFile arg.TextDocument.Uri
            return ()
        }

    override _.TextDocumentDidChange(arg: DidChangeTextDocumentParams) : Async<unit> =
        async {
            log $"TextDocumentDidChange {arg.TextDocument.Uri}"

            let text =
                match arg.ContentChanges[0] with
                | U2.C1 change -> change.Text
                | U2.C2 change -> change.Text

            docs.AddOrUpdate(arg.TextDocument.Uri, text, (fun _ _ -> text)) |> ignore
            let! _ = parseFile arg.TextDocument.Uri
            return ()
        }

    override _.TextDocumentDidClose(arg: DidCloseTextDocumentParams) : Async<unit> =
        async {
            log $"TextDocumentDidClose {arg.TextDocument.Uri}"
            docs.TryRemove(arg.TextDocument.Uri) |> ignore
            parseResults.TryRemove(arg.TextDocument.Uri) |> ignore
            return ()
        }

    override _.TextDocumentDiagnostic(arg: DocumentDiagnosticParams) : AsyncLspResult<DocumentDiagnosticReport> =
        asyncResult {
            log $"TextDocumentDiagnostic {arg.TextDocument.Uri}"
            let! _, _, diagnotics = parseFileWithDiagnostic arg.TextDocument.Uri true

            return
                DocumentDiagnosticReport.C1
                    { RelatedFullDocumentDiagnosticReport.Kind = "full"
                      Items = diagnotics
                      RelatedDocuments = None
                      ResultId = None }
        }

[<EntryPoint>]
let main args =
    if args |> Array.contains "--wait-for-debugger" then
        while not System.Diagnostics.Debugger.IsAttached do
            System.Threading.Thread.Sleep(100)

    let logger =
        LoggerConfiguration()
            .MinimumLevel.Debug()
            .WriteTo.File(
                Path.Combine(Path.GetTempPath() + "FsYaccLspServerLog", "log.txt"),
                rollingInterval = RollingInterval.Day
            )
            .CreateLogger()

    let inStream = System.Console.OpenStandardInput()
    let outStream = System.Console.OpenStandardOutput()
    let logFile = File.AppendText(Path.Combine(AppContext.BaseDirectory, "server.log"))
    logFile.WriteLine("Server started")
    logFile.Flush()

    Server.start
        (defaultRequestHandlings ())
        inStream
        outStream
        (fun (notifier, request) ->
            { new LspClient() with
                member _.TextDocumentPublishDiagnostics(arg: PublishDiagnosticsParams) : Async<unit> =
                    async {
                        match! notifier "textDocument/publishDiagnostics" (arg) with
                        | Ok _ -> ()
                        | Error e -> logger.Error $"send textDocument/publishDiagnostics {arg} FAILED: {e}"
                    }

                member _.ToString() : string = base.ToString() })
        (fun client -> new YaccLspServer(client, logger))
        (fun handler -> new JsonRpc(handler))
    |> int
