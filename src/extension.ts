// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import path from "path";
import fs from "fs";
import * as vscode from "vscode";

import { LanguageClient, TransportKind } from "vscode-languageclient/node";

let client: LanguageClient;
let outputChannel: vscode.OutputChannel;

export function activate(context: vscode.ExtensionContext) {
  outputChannel = vscode.window.createOutputChannel("FsYaccLsp");

  let serverMain = findInPath("dotnet") ?? "dotnet";
  let args = [path.join(context.extensionPath, "dist", "server", "FsYacc.LanguageServer.dll")];

  outputChannel.append(
    `Going to start server with command ${serverMain}, ${args}, ${context.extensionPath}`
  );
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
  outputChannel?.dispose();
}

function findInPath(binname: string) {
  let pathparts = process.env["PATH"]!.split(path.delimiter);
  for (let i = 0; i < pathparts.length; i++) {
    let binpath = path.join(pathparts[i], binname);
    if (fs.existsSync(binpath)) {
      return binpath;
    }

    let binpath2 = binpath + ".exe";
    if (fs.existsSync(binpath2)) {
      return binpath2;
    }
  }
  return null;
}
