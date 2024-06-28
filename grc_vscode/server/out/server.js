"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const path = require("path");
const child_process_1 = require("child_process");
const node_1 = require("vscode-languageserver/node");
const vscode_languageserver_textdocument_1 = require("vscode-languageserver-textdocument");
const util_1 = require("util");
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
let validationTimeouts = {};
documents.onDidChangeContent(change => {
    debounceValidateTextDocument(change.document);
});
const unlink = (0, util_1.promisify)(fs.unlink);
function debounceValidateTextDocument(textDocument) {
    const uri = textDocument.uri;
    if (validationTimeouts[uri]) {
        clearTimeout(validationTimeouts[uri]);
    }
    validationTimeouts[uri] = setTimeout(() => {
        validateTextDocument(textDocument);
        delete validationTimeouts[uri];
    }, 300); // Adjust the debounce delay as needed
}
async function validateTextDocument(textDocument) {
    const settings = await getDocumentSettings(textDocument.uri);
    const text = textDocument.getText();
    const tempFile = path.join(__dirname, 'temp_code.grc');
    try {
        fs.writeFileSync(tempFile, text);
        (0, child_process_1.execFile)('node', [path.join(__dirname, 'ocaml/main.js'), tempFile], async (error, stdout, stderr) => {
            if (error) {
                connection.console.error(`Execution error: ${stderr}`);
                return;
            }
            const diagnostics = stdout.trim().split('\n').map(line => {
                const match = line.match(/^\u001b\[31mError:\u001b\[0m (.+) at line (\d+)\.$/);
                if (!match)
                    return null;
                const [_, message, lineNumberStr] = match;
                const lineNumber = parseInt(lineNumberStr, 10) - 1;
                // Get the line text
                const lineText = textDocument.getText({
                    start: { line: lineNumber, character: 0 },
                    end: { line: lineNumber + 1, character: 0 }
                });
                return {
                    severity: node_1.DiagnosticSeverity.Error,
                    range: {
                        start: { line: lineNumber, character: 0 },
                        end: { line: lineNumber, character: lineText.length }
                    },
                    message: message.trim(),
                    source: 'ocaml'
                };
            }).filter(diagnostic => diagnostic !== null);
            connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
            // Print success message if no errors
            if (diagnostics.length === 0) {
                connection.console.log('Success: No errors found.');
            }
            // Clean up the temporary file
            try {
                await unlink(tempFile);
            }
            catch (cleanupErr) {
                if (cleanupErr.code !== 'ENOENT') {
                    connection.console.error(`Cleanup error: ${cleanupErr.message}`);
                }
            }
        });
    }
    catch (err) {
        connection.console.error(`File handling error: ${err.message}`);
        try {
            if (fs.existsSync(tempFile)) {
                await unlink(tempFile);
            }
        }
        catch (cleanupErr) {
            connection.console.error(`Cleanup error: ${cleanupErr.message}`);
        }
    }
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
connection.listen();
//# sourceMappingURL=server.js.map