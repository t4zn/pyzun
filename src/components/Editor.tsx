'use client';

import { Editor } from '@monaco-editor/react';
import { useTheme } from './ThemeProvider';

interface EditorProps {
  value: string;
  onChange: (value: string | undefined) => void;
  language: string;
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

export default function CodeEditor({ value, onChange, language }: EditorProps) {
  const { theme } = useTheme();

  return (
    <div className="h-full overflow-hidden">
      <Editor
        height="100%"
        language={languageMap[language] || 'python'}
        value={value}
        onChange={onChange}
        theme={theme === 'dark' ? 'vs-dark' : 'vs-light'}
        options={{
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
        }}
      />
    </div>
  );
}