"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const path = require("path");
const child_process_1 = require("child_process");
const node_1 = require("vscode-languageserver/node");
const vscode_languageserver_textdocument_1 = require("vscode-languageserver-textdocument");
const connection = (0, node_1.createConnection)(node_1.ProposedFeatures.all);
const documents = new node_1.TextDocuments(vscode_languageserver_textdocument_1.TextDocument);
let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;
connection.onInitialize((params) => {
    const capabilities = params.capabilities;
    hasConfigurationCapability = !!(capabilities.workspace && !!capabilities.workspace.configuration);
    hasWorkspaceFolderCapability = !!(capabilities.workspace && !!capabilities.workspace.workspaceFolders);
    hasDiagnosticRelatedInformationCapability = !!(capabilities.textDocument &&
        capabilities.textDocument.publishDiagnostics &&
        capabilities.textDocument.publishDiagnostics.relatedInformation);
    const result = {
        capabilities: {
            textDocumentSync: node_1.TextDocumentSyncKind.Incremental,
            completionProvider: {
                resolveProvider: true
            }
        }
    };
    if (hasWorkspaceFolderCapability) {
        result.capabilities.workspace = {
            workspaceFolders: {
                supported: true
            }
        };
    }
    return result;
});
connection.onInitialized(() => {
    if (hasConfigurationCapability) {
        connection.client.register(node_1.DidChangeConfigurationNotification.type, undefined);
    }
    if (hasWorkspaceFolderCapability) {
        connection.workspace.onDidChangeWorkspaceFolders(_event => {
            connection.console.log('Workspace folder change event received.');
        });
    }
});
const defaultSettings = { maxNumberOfProblems: 1000 };
let globalSettings = defaultSettings;
const documentSettings = new Map();
connection.onDidChangeConfiguration(change => {
    if (hasConfigurationCapability) {
        documentSettings.clear();
    }
    else {
        globalSettings = ((change.settings.languageServerExample || defaultSettings));
    }
    documents.all().forEach(validateTextDocument);
});
function getDocumentSettings(resource) {
    if (!hasConfigurationCapability) {
        return Promise.resolve(globalSettings);
    }
    let result = documentSettings.get(resource);
    if (!result) {
        result = connection.workspace.getConfiguration({
            scopeUri: resource,
            section: 'languageServerExample'
        });
        documentSettings.set(resource, result);
    }
    return result;
}
documents.onDidClose(e => {
    documentSettings.delete(e.document.uri);
    connection.sendDiagnostics({ uri: e.document.uri, diagnostics: [] });
});
documents.onDidChangeContent(change => {
    validateTextDocument(change.document);
});
async function validateTextDocument(textDocument) {
    const settings = await getDocumentSettings(textDocument.uri);
    const text = textDocument.getText();
    const tempFile = path.join(__dirname, '..', 'temp_code.grc'); // Adjusted file path
    // Write the text content to a temporary file
    fs.writeFileSync(tempFile, text);
    // Execute the OCaml code to analyze the file
    (0, child_process_1.execFile)('node', [path.join(__dirname, 'ocaml', 'main.js'), tempFile], (error, stdout, stderr) => {
        if (error) {
            connection.console.error(`Error: ${stderr}`);
            return;
        }
        const diagnostics = stdout.trim().split('\n').map(line => {
            const match = line.match(/^Error: (.+) at (\d+)-(\d+)$/);
            if (!match)
                return null;
            const [_, message, startPos, endPos] = match;
            return {
                severity: node_1.DiagnosticSeverity.Error,
                range: {
                    start: textDocument.positionAt(parseInt(startPos, 10)),
                    end: textDocument.positionAt(parseInt(endPos, 10))
                },
                message: message.trim(),
                source: 'ocaml'
            };
        }).filter(diagnostic => diagnostic !== null);
        // Log the diagnostics
        console.log(JSON.stringify(diagnostics, null, 2));
        connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
        // Clean up the temporary file if it exists
        if (fs.existsSync(tempFile)) {
            fs.unlinkSync(tempFile);
        }
    });
}
connection.onDidChangeWatchedFiles(_change => {
    connection.console.log('We received a file change event');
});
connection.onCompletion((_textDocumentPosition) => {
    return [
        {
            label: 'TypeScript',
            kind: node_1.CompletionItemKind.Text,
            data: 1
        },
        {
            label: 'JavaScript',
            kind: node_1.CompletionItemKind.Text,
            data: 2
        }
    ];
});
connection.onCompletionResolve((item) => {
    if (item.data === 1) {
        item.detail = 'TypeScript details';
        item.documentation = 'TypeScript documentation';
    }
    else if (item.data === 2) {
        item.detail = 'JavaScript details';
        item.documentation = 'JavaScript documentation';
    }
    return item;
});
documents.listen(connection);
//# sourceMappingURL=server.js.map