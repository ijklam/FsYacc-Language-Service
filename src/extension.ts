// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import path from "path";
import fs from "fs";
import * as vscode from "vscode";

import { LanguageClient, TransportKind } from "vscode-languageclient/node";

let client: LanguageClient | null = null;
let outputChannel: vscode.OutputChannel;

export async function activate(context: vscode.ExtensionContext) {
  outputChannel = vscode.window.createOutputChannel("FsYaccLsp");

  vscode.commands.registerCommand("fsyacc-language-service.stopLsp", () => {
    client?.stop();
    client = null;
  });
  vscode.commands.registerCommand(
    "fsyacc-language-service.startLsp",
    async () => await startLsp(context)
  );
  vscode.commands.registerCommand(
    "fsyacc-language-service.startLspWaitingDebugger",
    async () => await startLsp(context, "--wait-for-debugger")
  );

  await startLsp(context);
}

// This method is called when your extension is deactivated
export function deactivate() {
  client?.stop();
  outputChannel?.dispose();
}

async function startLsp(
  context: vscode.ExtensionContext,
  ...additionalArgs: string[]
) {
  if (client) {
    return;
  }

  const acquireResult: { dotnetPath: string } =
    await vscode.commands.executeCommand("dotnet.acquire", {
      version: "9.0",
      requestingExtensionId: context.extension.id,
    });

  const serverMain = acquireResult.dotnetPath;
  const args = [
    path.join(
      context.extensionPath,
      "dist",
      "server",
      "FsYacc.LanguageServer.dll"
    ),
    ...additionalArgs,
  ];

  outputChannel.append(
    `Going to start server with command ${serverMain}, ${args}, ${context.extensionPath}`
  );
  client = new LanguageClient(
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
      diagnosticCollectionName: "FsYacc",
      outputChannel,
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
