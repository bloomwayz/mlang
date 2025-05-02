import * as path from 'path';
import { workspace, ExtensionContext, commands } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
	Executable
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	const command = context.asAbsolutePath(
		path.join('_build', 'default', 'lsp', 'server', 'bin', 'main.exe')
	);

    const serverOptions: ServerOptions = {
        run: { command, transport: TransportKind.stdio },
        debug: { command, transport: TransportKind.stdio }
    };

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for M documents (*.m)
		documentSelector: [{ scheme: 'file', language: 'mlang'}],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		}
		
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'mlsp',
		'M Language Server',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}