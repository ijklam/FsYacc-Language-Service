// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import path from "path";
import fs from "fs";
import * as vscode from "vscode";

import { LanguageClient, TransportKind } from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
  let serverMain = findInPath("dotnet") ?? "dotnet";
  let args = [
    path.join(
      ..."out\\server\\FsYacc.LanguageServer.dll".split(
        "\\"
      )
    ),
  ];

  console.log("Going to start server with command  ", serverMain, args, context.extensionPath);
  let client = new LanguageClient(
    "FsYacc",
    {
      command: serverMain,
      args: args,
      transport: TransportKind.stdio,
      options: {
        cwd: context.extensionPath,
        env: {
          ...process.env,
        },
      },
    },
    {
      documentSelector: [
        { scheme: "file", language: "fsyacc" },
        { scheme: "untitled", language: "fsyacc" },
      ],
      synchronize: {
        // Notify the server about file changes to '.clientrc files contained in the workspace
        fileEvents: vscode.workspace.createFileSystemWatcher("**/.clientrc"),
      },
    }
  );
  client.start();
}

// This method is called when your extension is deactivated
export function deactivate() {
  client?.stop();
}

function findInPath(binname: string) {
  let pathparts = process.env["PATH"]!.split(path.delimiter);
  for (let i = 0; i < pathparts.length; i++) {
    let binpath = path.join(pathparts[i], binname);
    if (fs.existsSync(binpath)) {
      return binpath;
    }
  }
  return null;
}
