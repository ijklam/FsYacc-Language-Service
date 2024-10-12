# fsyacc-language-service README

Provide simple language service for FsYacc (.fsy files).

## Features

1. ToolTip and InlayHint
    - Hover on a token or rule name to see the definition.
    - Inlay hints are shown for `$` variables in code blocks.
![alt text](images/ToolTipAndInlayHint.png)

2. Goto definition and Find references
![alt text](images/GotoDefinitionAndFindReferences.png)

3. Auto completion
    - Outside code blocks:
    ![alt text](images/OutsideCodeBlocks.png)
    - Inside code blocks:
    ![alt text](images/InsideCodeBlocks.png)

4. Document outline
![alt text](images/DocumentOutline.png)

## Requirements

.NET Core 3.0 or later.

<!-- ## Extension Settings

Include if your extension adds any VS Code settings through the `contributes.configuration` extension point.

For example:

This extension contributes the following settings:

* `myExtension.enable`: Enable/disable this extension.
* `myExtension.thing`: Set to `blah` to do something. -->

## Known Issues

Document outline and Inlay hints may fail while editing.

## Release Notes

### 1.0.0

Initial release.
