'use client';

import { Editor } from '@monaco-editor/react';
import { useTheme } from './ThemeProvider';
import { useEffect, useRef, useCallback } from 'react';
import * as monaco from 'monaco-editor';

interface EditorProps {
  value: string;
  onChange: (value: string | undefined) => void;
  language: string;
  suggestedCode?: string;
  showDiff?: boolean;
  onApplyDiff?: () => void;
  onRejectDiff?: () => void;
}

const languageMap: Record<string, string> = {
  'assembly': 'asm',
  'bash': 'shell',
  'basic': 'vb',
  'c': 'c',
  'cpp': 'cpp',
  'csharp': 'csharp',
  'clojure': 'clojure',
  'cobol': 'cobol',
  'd': 'd',
  'elixir': 'elixir',
  'erlang': 'erlang',
  'fortran': 'fortran',
  'go': 'go',
  'haskell': 'haskell',
  'java': 'java',
  'javascript': 'javascript',
  'kotlin': 'kotlin',
  'lisp': 'lisp',
  'lua': 'lua',
  'objective_c': 'objective-c',
  'ocaml': 'ocaml',
  'octave': 'matlab',
  'pascal': 'pascal',
  'perl': 'perl',
  'php': 'php',
  'prolog': 'prolog',
  'python': 'python',
  'r': 'r',
  'ruby': 'ruby',
  'rust': 'rust',
  'scala': 'scala',
  'sql': 'sql',
  'swift': 'swift',
  'typescript': 'typescript',
  'visual_basic': 'vb'
};

export default function CodeEditor({ 
  value, 
  onChange, 
  language, 
  suggestedCode, 
  showDiff, 
  onApplyDiff, 
  onRejectDiff 
}: EditorProps) {
  const { theme } = useTheme();
  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);
  const monacoRef = useRef<typeof monaco | null>(null);

  const applyDiffDecorations = useCallback(() => {
    if (!editorRef.current || !monacoRef.current || !showDiff || !suggestedCode || !value) {
      return;
    }

    const originalLines = value.split('\n');
    const suggestedLines = suggestedCode.split('\n');
    const decorations: monaco.editor.IModelDeltaDecoration[] = [];
    
    // Show suggested code with highlighting for changes
    for (let i = 0; i < suggestedLines.length; i++) {
      const originalLine = originalLines[i];
      const suggestedLine = suggestedLines[i];
      
      if (originalLine !== suggestedLine) {
        const isAdded = originalLine === undefined;
        const isModified = originalLine !== undefined && suggestedLine !== undefined;
        
        if (isAdded || isModified) {

          
          decorations.push({
            range: new monacoRef.current.Range(i + 1, 1, i + 1, 1),
            options: {
              isWholeLine: true,
              className: 'diff-highlight-line',
              overviewRuler: {
                color: '#22c55e',
                position: monacoRef.current.editor.OverviewRulerLane.Left
              },
              minimap: {
                color: '#22c55e',
                position: monacoRef.current.editor.MinimapPosition.Inline
              }
            }
          });
          
          // Add inline decoration for the entire line
          decorations.push({
            range: new monacoRef.current.Range(i + 1, 1, i + 1, suggestedLine.length + 1),
            options: {
              inlineClassName: 'diff-inline-highlight',
              stickiness: monacoRef.current.editor.TrackedRangeStickiness.NeverGrowsWhenTypingAtEdges
            }
          });
        }
      }
    }
    
    editorRef.current.deltaDecorations([], decorations);
  }, [showDiff, suggestedCode, value]);

  const handleEditorDidMount = (editor: monaco.editor.IStandaloneCodeEditor, monacoInstance: typeof monaco) => {
    editorRef.current = editor;
    monacoRef.current = monacoInstance;
    applyDiffDecorations();
  };

  useEffect(() => {
    applyDiffDecorations();
  }, [showDiff, suggestedCode, value, applyDiffDecorations]);

  return (
    <div className="h-full overflow-hidden relative">
      {showDiff && suggestedCode && (
        <div className="absolute top-3 right-3 z-10 flex gap-1">
          <button
            onClick={onApplyDiff}
            className="w-6 h-6 bg-green-500 hover:bg-green-600 text-white text-xs rounded flex items-center justify-center transition-colors"
            title="Apply changes"
          >
            ✓
          </button>
          <button
            onClick={onRejectDiff}
            className="w-6 h-6 bg-gray-500 hover:bg-gray-600 text-white text-xs rounded flex items-center justify-center transition-colors"
            title="Reject changes"
          >
            ✕
          </button>
        </div>
      )}
      <Editor
        height="100%"
        language={languageMap[language] || 'python'}
        value={showDiff && suggestedCode ? suggestedCode : value}
        onChange={onChange}
        onMount={(editor, monaco) => {
          handleEditorDidMount(editor, monaco);
          
          // Ensure all features are enabled after mount
          editor.updateOptions({
            quickSuggestions: {
              other: true,
              comments: true,
              strings: true
            },
            suggestOnTriggerCharacters: true,
            acceptSuggestionOnCommitCharacter: true,
            acceptSuggestionOnEnter: 'on'
          });

          // Add keyboard shortcuts for common actions
          editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.Space, () => {
            editor.trigger('keyboard', 'editor.action.triggerSuggest', {});
          });

          // Force suggestions to show on Ctrl+Space
          editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.Space, () => {
            editor.trigger('keyboard', 'editor.action.triggerParameterHints', {});
          });
        }}
        theme={theme === 'dark' ? 'vs-dark' : 'vs-light'}
        options={{
          // Basic editor options
          minimap: { enabled: false },
          fontSize: 15,
          lineNumbers: 'on',
          roundedSelection: false,
          scrollBeyondLastLine: false,
          automaticLayout: true,
          tabSize: 2,
          insertSpaces: true,
          wordWrap: 'on',
          fontFamily: 'var(--font-geist-mono), monospace',
          lineHeight: 1.6,
          padding: { top: 20, bottom: 20 },
          renderLineHighlight: 'none',
          hideCursorInOverviewRuler: true,
          overviewRulerBorder: false,
          scrollbar: {
            vertical: 'hidden',
            horizontal: 'hidden',
          },

          // Enhanced IntelliSense and autocompletion
          quickSuggestions: {
            other: true,
            comments: true,
            strings: true
          },
          suggestOnTriggerCharacters: true,
          acceptSuggestionOnCommitCharacter: true,
          acceptSuggestionOnEnter: 'on',
          wordBasedSuggestions: 'allDocuments',
          parameterHints: {
            enabled: true,
            cycle: true
          },

          // Code completion and snippets
          snippetSuggestions: 'top',
          tabCompletion: 'on',
          suggest: {
            insertMode: 'replace',
            filterGraceful: true,
            showKeywords: true,
            showSnippets: true,
            showFunctions: true,
            showConstructors: true,
            showFields: true,
            showVariables: true,
            showClasses: true,
            showStructs: true,
            showInterfaces: true,
            showModules: true,
            showProperties: true,
            showEvents: true,
            showOperators: true,
            showUnits: true,
            showValues: true,
            showConstants: true,
            showEnums: true,
            showEnumMembers: true,
            showColors: true,
            showFiles: true,
            showReferences: true,
            showFolders: true,
            showTypeParameters: true
          },

          // Error detection and validation (handled by language services)

          // Bracket matching and auto-closing
          matchBrackets: 'always',
          autoClosingBrackets: 'always',
          autoClosingQuotes: 'always',
          autoSurround: 'languageDefined',

          // Code formatting
          formatOnPaste: true,
          formatOnType: true,

          // Hover information
          hover: {
            enabled: true,
            delay: 300,
            sticky: true
          },

          // Code lens
          codeLens: true,

          // Folding
          folding: true,
          foldingStrategy: 'auto',
          showFoldingControls: 'mouseover',

          // Find and replace
          find: {
            addExtraSpaceOnTop: false,
            autoFindInSelection: 'never',
            seedSearchStringFromSelection: 'always'
          }
        }}
        beforeMount={(monaco) => {
          // Configure TypeScript/JavaScript language features
          monaco.languages.typescript.javascriptDefaults.setEagerModelSync(true);
          monaco.languages.typescript.javascriptDefaults.setDiagnosticsOptions({
            noSemanticValidation: false,
            noSyntaxValidation: false,
            noSuggestionDiagnostics: false
          });

          monaco.languages.typescript.typescriptDefaults.setEagerModelSync(true);
          monaco.languages.typescript.typescriptDefaults.setDiagnosticsOptions({
            noSemanticValidation: false,
            noSyntaxValidation: false,
            noSuggestionDiagnostics: false
          });

          // Configure compiler options for better IntelliSense
          monaco.languages.typescript.javascriptDefaults.setCompilerOptions({
            target: monaco.languages.typescript.ScriptTarget.ES2020,
            allowNonTsExtensions: true,
            moduleResolution: monaco.languages.typescript.ModuleResolutionKind.NodeJs,
            module: monaco.languages.typescript.ModuleKind.CommonJS,
            noEmit: true,
            esModuleInterop: true,
            jsx: monaco.languages.typescript.JsxEmit.React,
            reactNamespace: 'React',
            allowJs: true,
            typeRoots: ['node_modules/@types']
          });

          monaco.languages.typescript.typescriptDefaults.setCompilerOptions({
            target: monaco.languages.typescript.ScriptTarget.ES2020,
            allowNonTsExtensions: true,
            moduleResolution: monaco.languages.typescript.ModuleResolutionKind.NodeJs,
            module: monaco.languages.typescript.ModuleKind.CommonJS,
            noEmit: true,
            esModuleInterop: true,
            jsx: monaco.languages.typescript.JsxEmit.React,
            reactNamespace: 'React',
            allowJs: true,
            typeRoots: ['node_modules/@types']
          });

          // Enable Emmet for HTML, CSS, and related languages
          monaco.languages.registerCompletionItemProvider('html', {
            provideCompletionItems: (model, position) => {
              const word = model.getWordUntilPosition(position);
              const range = {
                startLineNumber: position.lineNumber,
                endLineNumber: position.lineNumber,
                startColumn: word.startColumn,
                endColumn: word.endColumn
              };

              return {
                suggestions: [
                  {
                    label: 'div',
                    kind: monaco.languages.CompletionItemKind.Snippet,
                    insertText: '<div>$1</div>',
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    documentation: 'HTML div element',
                    range: range
                  },
                  {
                    label: 'p',
                    kind: monaco.languages.CompletionItemKind.Snippet,
                    insertText: '<p>$1</p>',
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    documentation: 'HTML paragraph element',
                    range: range
                  }
                ]
              };
            }
          });

          // Add custom snippets for popular languages
          const addLanguageSnippets = (languageId: string, snippets: Omit<monaco.languages.CompletionItem, 'range' | 'kind'>[]) => {
            monaco.languages.registerCompletionItemProvider(languageId, {
              provideCompletionItems: (model, position) => {
                const word = model.getWordUntilPosition(position);
                const range = {
                  startLineNumber: position.lineNumber,
                  endLineNumber: position.lineNumber,
                  startColumn: word.startColumn,
                  endColumn: word.endColumn
                };

                return {
                  suggestions: snippets.map(snippet => ({
                    ...snippet,
                    kind: monaco.languages.CompletionItemKind.Snippet,
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    range: range
                  }))
                };
              }
            });
          };

          // Python snippets
          addLanguageSnippets('python', [
            {
              label: 'for',
              insertText: 'for ${1:item} in ${2:iterable}:\n    ${3:pass}',
              documentation: 'For loop',
              detail: 'Python for loop'
            },
            {
              label: 'if',
              insertText: 'if ${1:condition}:\n    ${2:pass}',
              documentation: 'If statement',
              detail: 'Python if statement'
            },
            {
              label: 'def',
              insertText: 'def ${1:function_name}(${2:parameters}):\n    ${3:pass}',
              documentation: 'Function definition',
              detail: 'Python function'
            },
            {
              label: 'class',
              insertText: 'class ${1:ClassName}:\n    def __init__(self${2:, parameters}):\n        ${3:pass}',
              documentation: 'Class definition',
              detail: 'Python class'
            },
            {
              label: 'try',
              insertText: 'try:\n    ${1:pass}\nexcept ${2:Exception} as ${3:e}:\n    ${4:pass}',
              documentation: 'Try-except block',
              detail: 'Python exception handling'
            },
            {
              label: 'while',
              insertText: 'while ${1:condition}:\n    ${2:pass}',
              documentation: 'While loop',
              detail: 'Python while loop'
            },
            {
              label: 'with',
              insertText: 'with ${1:expression} as ${2:variable}:\n    ${3:pass}',
              documentation: 'With statement',
              detail: 'Python context manager'
            }
          ]);

          // JavaScript/TypeScript snippets
          ['javascript', 'typescript'].forEach(lang => {
            addLanguageSnippets(lang, [
              {
                label: 'function',
                insertText: 'function ${1:functionName}(${2:parameters}) {\n    ${3:// code}\n}',
                documentation: 'Function declaration'
              },
              {
                label: 'arrow',
                insertText: 'const ${1:functionName} = (${2:parameters}) => {\n    ${3:// code}\n};',
                documentation: 'Arrow function'
              },
              {
                label: 'for',
                insertText: 'for (let ${1:i} = 0; ${1:i} < ${2:array}.length; ${1:i}++) {\n    ${3:// code}\n}',
                documentation: 'For loop'
              },
              {
                label: 'if',
                insertText: 'if (${1:condition}) {\n    ${2:// code}\n}',
                documentation: 'If statement'
              }
            ]);
          });

          // Java snippets
          addLanguageSnippets('java', [
            {
              label: 'main',
              insertText: 'public static void main(String[] args) {\n    ${1:// code}\n}',
              documentation: 'Main method'
            },
            {
              label: 'for',
              insertText: 'for (int ${1:i} = 0; ${1:i} < ${2:length}; ${1:i}++) {\n    ${3:// code}\n}',
              documentation: 'For loop'
            },
            {
              label: 'if',
              insertText: 'if (${1:condition}) {\n    ${2:// code}\n}',
              documentation: 'If statement'
            }
          ]);

          // C/C++ snippets
          ['c', 'cpp'].forEach(lang => {
            addLanguageSnippets(lang, [
              {
                label: 'main',
                insertText: 'int main() {\n    ${1:// code}\n    return 0;\n}',
                documentation: 'Main function',
                detail: 'C/C++ main function'
              },
              {
                label: 'for',
                insertText: 'for (int ${1:i} = 0; ${1:i} < ${2:n}; ${1:i}++) {\n    ${3:// code}\n}',
                documentation: 'For loop',
                detail: 'C/C++ for loop'
              },
              {
                label: 'if',
                insertText: 'if (${1:condition}) {\n    ${2:// code}\n}',
                documentation: 'If statement',
                detail: 'C/C++ if statement'
              },
              {
                label: 'include',
                insertText: '#include <${1:stdio.h}>',
                documentation: 'Include header',
                detail: 'C/C++ include directive'
              }
            ]);
          });

          // Add more aggressive suggestion triggers
          monaco.languages.setLanguageConfiguration('python', {
            wordPattern: /(-?\d*\.\d\w*)|([^\`\~\!\@\#\%\^\&\*\(\)\-\=\+\[\{\]\}\\\|\;\:\'\"\,\.\<\>\/\?\s]+)/g,
            comments: {
              lineComment: '#',
              blockComment: ['"""', '"""']
            },
            brackets: [
              ['{', '}'],
              ['[', ']'],
              ['(', ')']
            ],
            autoClosingPairs: [
              { open: '{', close: '}' },
              { open: '[', close: ']' },
              { open: '(', close: ')' },
              { open: '"', close: '"', notIn: ['string'] },
              { open: "'", close: "'", notIn: ['string', 'comment'] }
            ],
            surroundingPairs: [
              { open: '{', close: '}' },
              { open: '[', close: ']' },
              { open: '(', close: ')' },
              { open: '"', close: '"' },
              { open: "'", close: "'" }
            ],
            folding: {
              offSide: true,
              markers: {
                start: new RegExp('^\\s*#region\\b'),
                end: new RegExp('^\\s*#endregion\\b')
              }
            }
          });
        }}
      />
    </div>
  );
}