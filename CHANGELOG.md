# Change Log

All notable changes to the "fsyacc-language-service" extension will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.

### 0.1.0
1. Support recognize the `%left`, `%right`, `%nonassoc` and `%prec` grammar rules. All the goto definitions feature, find references feature and the completion feature can work with these tokens.
2. Error reporting improved. It can now show the error message and error range from the parser, not just reporting the "parse failed".
3. Completion feature enhanced. It can provide different completions under different context. While editing a rule clause, it will provide the token completions.
4. Enhanced robustness. It can now active lsp features once the file opened or changed.
5. Update target framework to .NET 9.0. And it can now auto acquire the prefered runtime through the ms-dotnettools.vscode-dotnet-runtime extension.
6. Add some command to start and stop the language server.

### 0.0.3

Support Call Hierarchy to quickly find out symbol usages.

### 0.0.2

Update Readme.

### 0.0.1

Initial release.
