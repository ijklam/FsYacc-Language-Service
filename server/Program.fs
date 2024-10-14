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

type Config =
    { ShowDollarReferenceInFindingReferencesOfRulesOrTokens: bool }

    static member Default = { ShowDollarReferenceInFindingReferencesOfRulesOrTokens = false }

let makeCallHierarchyItem name token uri r =
    {
        CallHierarchyItem.Data = Some(Json.fromObject token)
        Name = name
        Kind = SymbolKind.Function
        Tags = None
        Detail = None
        Uri = uri
        Range = r
        SelectionRange = r
    }

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

    let docs = Dictionary()

    let parseFile uri =
        asyncResult {
            try
                let path = Uri(uri).LocalPath.[1..]
                let filename = System.IO.Path.GetFileName(path)
                let! file = System.IO.File.ReadAllTextAsync(path) |> Async.AwaitTask
                let hash' = hash file

                match docs.TryGetValue uri with
                | true, (hash'', spec, tokens) when hash' = hash'' -> return spec, tokens
                | _ ->
                    match FsLexYacc.FsYacc.Driver.readSpecFromString filename file with
                    | Ok spec ->
                        let symbols = FsLexYacc.FsYacc.Driver.specToSymbols spec
                        log $"parseFile {uri} \ntokensCount: {symbols.Length}"
                        docs.[uri] <- (hash', spec, symbols)
                        return spec, symbols
                    | Error err ->
                        match docs.TryGetValue uri with
                        | true, (_, spec, tokens) -> return spec, tokens
                        | _ -> return! Error err
            with ex ->
                log $"{ex}"
                return! Error "parseFile failed"
        }
        |> AsyncResult.mapError JsonRpc.Error.InternalErrorMessage

    override this.Dispose() : unit = logger.Dispose()

    override __.Initialize(p) =
        log $"Initialize {p.RootUri}"
        let capabilities = p.Capabilities
        let hasConfigurationCapability = 
            capabilities.Workspace.IsSome && capabilities.Workspace.Value.Configuration.IsSome

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
                              WorkDoneProgress = Some false } } }
        |> Ok
        |> Async.retn

    override __.TextDocumentDefinition(arg: TextDocumentPositionParams) : AsyncLspResult<Definition option> =
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
            | FsYaccSymbolKind.Rule ->
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
        }

    override __.TextDocumentReferences(arg: ReferenceParams) : AsyncLspResult<Location array option> =
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

            match kind with
            | FsYaccSymbolKind.Token ->
                let ranges =
                    tokens
                    |> Array.choose (fun ((x, r), k) ->
                        match k with
                        // | FsYaccSymbolKind.DollarRef(x, _) when x = theToken -> Some({ Uri = uri; Range = r })
                        | FsYaccSymbolKind.Rule
                        | FsYaccSymbolKind.RuleClauseRef when x = theToken -> Some({ Uri = uri; Range = r })
                        | _ -> None)

                log $"TextDocumentReferences ranges: {ranges.Length}"
                return ranges

            | FsYaccSymbolKind.Type ->
                let ranges =
                    tokens
                    |> Array.choose (fun ((x, r), k) ->
                        if (k.IsType) && x = theToken then
                            Some({ Uri = uri; Range = r })
                        else
                            None)

                log $"TextDocumentReferences ranges: {ranges.Length}"
                return ranges

            | FsYaccSymbolKind.Rule ->
                let ranges =
                    tokens
                    |> Array.choose (fun ((x, r), k) ->
                        match k with
                        // | FsYaccSymbolKind.DollarRef(x, _) when x = theToken -> Some({ Uri = uri; Range = r })
                        | FsYaccSymbolKind.RuleClauseRef when x = theToken -> Some({ Uri = uri; Range = r })
                        | _ -> None)

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
        }

    override this.TextDocumentHover(arg: TextDocumentPositionParams) : AsyncLspResult<Hover option> =
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

            let r =
                { Contents =
                    U3.C3(
                        [| match kind with
                           | FsYaccSymbolKind.Type ->
                               U2.C2
                                   { Language = "FsYacc"
                                     Value = $"type {theToken}" }
                           | FsYaccSymbolKind.Token ->
                               U2.C2
                                   { Language = "FsYacc"
                                     Value = $"%%token {theToken}" }
                           | FsYaccSymbolKind.Rule ->
                               let _, clauses = spec.Rules |> List.find (fun ((x, _), _) -> x = theToken)

                               seq {
                                   $"{theToken}: "

                                   for FsLexYacc.FsYacc.AST.Rule(definitions, _, _) in clauses do
                                       let definitions = definitions |> List.map fst
                                       let definitions = String.concat " " definitions
                                       $"  | {definitions}"
                               }
                               |> String.concat "\n"
                               |> fun i -> U2.C2 { Language = "FsYacc"; Value = i }
                           | FsYaccSymbolKind.RuleClauseRef
                           | FsYaccSymbolKind.DollarRef _ -> notPossible () |]
                    )
                  Range = None }

            log $"TextDocumentHover ({r})"
            return r
        }

    override this.TextDocumentDocumentSymbol
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
                    | FsYaccSymbolKind.RuleClauseRef -> None
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

    override this.TextDocumentInlayHint(arg: InlayHintParams) : AsyncLspResult<InlayHint array option> =
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

    override this.TextDocumentCompletion(arg: CompletionParams) : AsyncLspResult<CompletionList option> =
        let makeCompletion (spec: FsLexYacc.FsYacc.AST.ParserSpec) (x: string) k =
            match k with
            | FsYaccSymbolKind.Token ->
                { CompletionItem.Create(x) with
                    Kind = Some CompletionItemKind.Keyword
                    Detail = Some $"%%token {x}" }
            | FsYaccSymbolKind.Rule ->
                let _, clauses = spec.Rules |> List.find (fun ((x', _), _) -> x = x')

                let desc =
                    seq {
                        $"{x}: "

                        for FsLexYacc.FsYacc.AST.Rule(definitions, _, _) in clauses do
                            let definitions = definitions |> List.map fst
                            let definitions = String.concat " " definitions
                            $"  | {definitions}"
                    }
                    |> String.concat "\n"

                { CompletionItem.Create(x) with
                    Kind = Some CompletionItemKind.Function
                    Detail = Some desc }
            | FsYaccSymbolKind.DollarRef _
            | FsYaccSymbolKind.Type
            | FsYaccSymbolKind.RuleClauseRef -> notPossible ()

        let findCompletions spec (tokens: (_ * FsYaccSymbolKind) array) (nameMapper: _ array option) =
            let shouldMapName, nameMapper, oldnames =
                match nameMapper with
                | Some arr ->
                    let a, b = arr |> Array.unzip
                    true, arr, HashSet b
                | _ -> false, Array.empty, HashSet()

            let r =
                tokens
                |> Array.choose (fun ((x, r), k) ->
                    if not (k.IsRule || k.IsToken) then
                        None
                    elif shouldMapName then
                        if oldnames.Contains(x) then
                            Some(makeCompletion spec x k)
                        else
                            None
                    else
                        Some(makeCompletion spec x k))
                |> Array.distinctBy (fun x -> x.Label)

            let r =
                if shouldMapName then
                    let v = r |> Array.map (fun x -> x.Label, x) |> Map.ofArray
                    nameMapper |> Array.map (fun (x, y) -> { v.[y] with Label = x })
                else
                    r

            { IsIncomplete = false
              Items = r
              ItemDefaults = None }

        asyncResultOption {
            log $"TextDocumentCompletion {arg.TextDocument.Uri}"
            let uri = arg.TextDocument.Uri
            let! spec, tokens = parseFile uri

            let definitionsOpt =
                spec.Rules
                |> List.tryPick (fun (_, ls) ->
                    ls
                    |> List.tryPick (fun (FsLexYacc.FsYacc.AST.Rule(definitions, _, code)) ->
                        match code with
                        | Some(_, range) when posInRange arg.Position range -> Some definitions
                        | _ -> None))

            match definitionsOpt with
            | None -> return findCompletions spec tokens None
            | Some definitions ->
                let definitions =
                    definitions |> List.mapi (fun i (x, _) -> $"${i + 1}", x) |> List.toArray

                return findCompletions spec tokens (Some definitions)
        }

    override __.TextDocumentPrepareCallHierarchy (arg: CallHierarchyPrepareParams): AsyncLspResult<CallHierarchyItem array option> = 
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
            | FsYaccSymbolKind.Rule -> 
                return [| makeCallHierarchyItem theToken (theToken, r) uri r |]
            | FsYaccSymbolKind.RuleClauseRef -> return! None
            | FsYaccSymbolKind.Type
            | FsYaccSymbolKind.DollarRef _ -> return! None
        }

    override __.CallHierarchyIncomingCalls (arg: CallHierarchyIncomingCallsParams): AsyncLspResult<CallHierarchyIncomingCall array option> = 
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
                    |> List.collect (fun ((ruleName, ruleRange), j) -> 
                        j
                        |> List.indexed
                        |> List.choose (fun (i, (FsLexYacc.FsYacc.AST.Rule(definitions, _, _))) -> 
                            if definitions.IsEmpty then None else
                            let ranges =
                                definitions 
                                |> List.choose (fun (x, r) -> if x = theToken then Some r else None)
                                |> Array.ofList
                            let rr = definitions.Head |> snd
                            match ranges with
                            | [||] -> None
                            | _ -> 
                                Some {
                                    CallHierarchyIncomingCall.From = makeCallHierarchyItem $"{ruleName} #{i + 1}" (ruleName, ruleRange) uri rr
                                    FromRanges = ranges
                                })
                    )
                return r |> Array.ofList
            | FsYaccSymbolKind.RuleClauseRef -> return! None
            | FsYaccSymbolKind.Type
            | FsYaccSymbolKind.DollarRef _ -> return! None
   
        }


    override __.CallHierarchyOutgoingCalls (arg: CallHierarchyOutgoingCallsParams): AsyncLspResult<CallHierarchyOutgoingCall array option> = 
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
                    |> List.filter (fun ((x, _), _) -> x = theToken)
                    |> List.collect (fun ((ruleName, ruleRange), j) -> 
                        j
                        |> List.collect (fun ((FsLexYacc.FsYacc.AST.Rule(definitions, _, _))) -> definitions)
                        |> List.groupBy fst
                        |> List.map (fun (x, xs) ->
                            let ranges = xs |> List.toArray |> Array.map snd
                            {
                                CallHierarchyOutgoingCall.To = makeCallHierarchyItem x (x, ranges[0]) uri ruleRange
                                FromRanges = ranges
                            }
                        )
                    )
                return r |> Array.ofList
            | FsYaccSymbolKind.Token
            | FsYaccSymbolKind.RuleClauseRef
            | FsYaccSymbolKind.Type
            | FsYaccSymbolKind.DollarRef _ -> return! None
       
        }

let startServer () =
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
        (fun _ ->
            { new LspClient() with
                member _.ToString() : string = base.ToString() })
        (fun client -> new YaccLspServer(client, logger))
        (fun handler -> new JsonRpc(handler))

ignore (startServer ())
