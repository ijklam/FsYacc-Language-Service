
module FsLexYacc.FsYacc.Lexer
open FsLexYacc.FsYacc.AST
open FsLexYacc.FsYacc.Parser
open System.Text
open FSharp.Text.Lexing
open TextUtils/// Rule token
val token: lexbuf: LexBuffer<char> -> token
/// Rule fs_type
val fs_type: p: obj -> lexbuf: LexBuffer<char> -> token
/// Rule header
val header: p: obj -> buff: obj -> lexbuf: LexBuffer<char> -> token
/// Rule code
val code: p: obj -> buff: obj -> lexbuf: LexBuffer<char> -> token
/// Rule codestring
val codestring: buff: obj -> lexbuf: LexBuffer<char> -> token
/// Rule comment
val comment: lexbuf: LexBuffer<char> -> token
