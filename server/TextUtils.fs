module TextUtils
open Ionide.LanguageServerProtocol.Types

let inline (!) x = not x

module Json =
    let fromObject (obj: 'a) =
        Newtonsoft.Json.Linq.JToken.FromObject(
            obj,
            Ionide.LanguageServerProtocol.Server.jsonRpcFormatter.JsonSerializer
        )

type ParserPos = FSharp.Text.Lexing.Position
type ParserRange = ParserPos * ParserPos

type LspPos = Ionide.LanguageServerProtocol.Types.Position
type LspRange = Ionide.LanguageServerProtocol.Types.Range
let emptyLspPos = { Line = 0u; Character = 0u }
let emptyRange = { Start = emptyLspPos; End = emptyLspPos }

let parserPosToLspPos (pos: ParserPos) : LspPos =
    { Line = uint (pos.Line - 1)
      Character = uint pos.Column }

let posInRange (pos: LspPos) (range: LspRange) : bool =
    let start, endPos = range.Start, range.End
    start.Line <= pos.Line && pos.Line <= endPos.Line &&
    start.Character <= pos.Character && pos.Character <= endPos.Character

let parserRangeToLspRange (range: ParserRange) : LspRange =
    let start, endPos = range
    { Start = parserPosToLspPos start
      End = parserPosToLspPos endPos }

type PosHelper(text: string, startLine) =
    let newlines =
        [| for i in 0 .. text.Length - 1 do
               if text.[i] = '\n' then
                   uint32 i
           uint32 text.Length |]

    member _.Text = text

    member _.GetPos idx : LspPos =
        let i = Array.findIndex ((<=) idx) newlines
        { Line = uint i + startLine
          Character = idx - (if i = 0 then 0u else newlines.[i - 1] + 1u) }

    member this.FindRange (value: string) (startIdx: int) : LspRange =
        let idx = text.IndexOf(value, startIdx)
        let pos = this.GetPos(uint32 idx)
        { Start = pos
          End = { pos with Character = pos.Character + uint32 value.Length } }
