import * as fs from 'fs';
import * as path from 'path';
import { execFile } from 'child_process';
import {
  createConnection,
  TextDocuments,
  Diagnostic,
  DiagnosticSeverity,
  ProposedFeatures,
  InitializeParams,
  DidChangeConfigurationNotification,
  CompletionItem,
  CompletionItemKind,
  TextDocumentPositionParams,
  TextDocumentSyncKind,
  InitializeResult
} from 'vscode-languageserver/node';

import {
  TextDocument
} from 'vscode-languageserver-textdocument';
import { promisify } from 'util';

const connection = createConnection(ProposedFeatures.all);
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

connection.onInitialize((params: InitializeParams) => {
  const capabilities = params.capabilities;

  hasConfigurationCapability = !!(
    capabilities.workspace && !!capabilities.workspace.configuration
  );
  hasWorkspaceFolderCapability = !!(
    capabilities.workspace && !!capabilities.workspace.workspaceFolders
  );
  hasDiagnosticRelatedInformationCapability = !!(
    capabilities.textDocument &&
    capabilities.textDocument.publishDiagnostics &&
    capabilities.textDocument.publishDiagnostics.relatedInformation
  );

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
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
    connection.client.register(DidChangeConfigurationNotification.type, undefined);
  }
  if (hasWorkspaceFolderCapability) {
    connection.workspace.onDidChangeWorkspaceFolders(_event => {
      connection.console.log('Workspace folder change event received.');
    });
  }
});

interface ExampleSettings {
  maxNumberOfProblems: number;
}

const defaultSettings: ExampleSettings = { maxNumberOfProblems: 1000 };
let globalSettings: ExampleSettings = defaultSettings;

const documentSettings: Map<string, Thenable<ExampleSettings>> = new Map();

connection.onDidChangeConfiguration(change => {
  if (hasConfigurationCapability) {
    documentSettings.clear();
  } else {
    globalSettings = <ExampleSettings>(
      (change.settings.languageServerExample || defaultSettings)
    );
  }
  documents.all().forEach(validateTextDocument);
});

function getDocumentSettings(resource: string): Thenable<ExampleSettings> {
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

let validationTimeouts: { [uri: string]: NodeJS.Timeout } = {};

documents.onDidChangeContent(change => {
  debounceValidateTextDocument(change.document);
});

const unlink = promisify(fs.unlink);

function debounceValidateTextDocument(textDocument: TextDocument): void {
  const uri = textDocument.uri;
  if (validationTimeouts[uri]) {
    clearTimeout(validationTimeouts[uri]);
  }
  validationTimeouts[uri] = setTimeout(() => {
    validateTextDocument(textDocument);
    delete validationTimeouts[uri];
  }, 300); // Adjust the debounce delay as needed
}

async function validateTextDocument(textDocument: TextDocument): Promise<void> {
  const settings = await getDocumentSettings(textDocument.uri);
  const text = textDocument.getText();
  const tempFile = path.join(__dirname, 'temp_code.grc');

  try {
    fs.writeFileSync(tempFile, text);
    execFile('node', [path.join(__dirname, 'ocaml/main.js'), tempFile], async (error, stdout, stderr) => {
      if (error) {
        connection.console.error(`Execution error: ${stderr}`);
        return;
      }

      const diagnostics: Diagnostic[] = stdout.trim().split('\n').map(line => {
        const match = line.match(/^\u001b\[31mError:\u001b\[0m (.+) at line (\d+)\.$/);
        if (!match) return null;

        const [_, message, lineNumberStr] = match;
        const lineNumber = parseInt(lineNumberStr, 10) - 1;

        // Get the line text
        const lineText = textDocument.getText({
          start: { line: lineNumber, character: 0 },
          end: { line: lineNumber + 1, character: 0 }
        });

        return {
          severity: DiagnosticSeverity.Error,
          range: {
            start: { line: lineNumber, character: 0 },
            end: { line: lineNumber, character: lineText.length }
          },
          message: message.trim(),
          source: 'ocaml'
        };
      }).filter(diagnostic => diagnostic !== null) as Diagnostic[];

      connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });

      // Print success message if no errors
      if (diagnostics.length === 0) {
        connection.console.log('Success: No errors found.');
      }

      // Clean up the temporary file
      try {
        await unlink(tempFile);
      } catch (cleanupErr) {
        if ((cleanupErr as NodeJS.ErrnoException).code !== 'ENOENT') {
          connection.console.error(`Cleanup error: ${(cleanupErr as NodeJS.ErrnoException).message}`);
        }
      }
    });
  } catch (err) {
    connection.console.error(`File handling error: ${(err as Error).message}`);
    try {
      if (fs.existsSync(tempFile)) {
        await unlink(tempFile);
      }
    } catch (cleanupErr) {
      connection.console.error(`Cleanup error: ${(cleanupErr as Error).message}`);
    }
  }
}

connection.onDidChangeWatchedFiles(_change => {
  connection.console.log('We received a file change event');
});

connection.onCompletion(
  (_textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
    return [
      {
        label: 'TypeScript',
        kind: CompletionItemKind.Text,
        data: 1
      },
      {
        label: 'JavaScript',
        kind: CompletionItemKind.Text,
        data: 2
      }
    ];
  }
);

connection.onCompletionResolve(
  (item: CompletionItem): CompletionItem => {
    if (item.data === 1) {
      item.detail = 'TypeScript details';
      item.documentation = 'TypeScript documentation';
    } else if (item.data === 2) {
      item.detail = 'JavaScript details';
      item.documentation = 'JavaScript documentation';
    }
    return item;
  }
);

documents.listen(connection);
connection.listen();
