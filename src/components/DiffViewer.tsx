'use client';

import { DiffEditor } from '@monaco-editor/react';
import { useTheme } from './ThemeProvider';

interface DiffViewerProps {
  originalCode: string;
  modifiedCode: string;
  language: string;
  onApply: () => void;
  onReject: () => void;
}

export function DiffViewer({ 
  originalCode, 
  modifiedCode, 
  language, 
  onApply, 
  onReject 
}: DiffViewerProps) {
  const { theme } = useTheme();

  return (
    <div className="fixed inset-0 bg-black/60 backdrop-blur-sm flex items-center justify-center z-50 p-4">
      <div className="bg-white dark:bg-black border border-gray-200 dark:border-gray-800 rounded-xl shadow-2xl w-full max-w-7xl h-[85vh] flex flex-col overflow-hidden">
        <div className="flex items-center justify-between px-6 py-4 border-b border-gray-200 dark:border-gray-800">
          <h3 className="text-lg font-medium text-black dark:text-white">
            AI Code Suggestion
          </h3>
          <div className="flex gap-3">
            <button
              onClick={onApply}
              className="px-5 py-2.5 bg-black dark:bg-white text-white dark:text-black rounded-lg font-medium hover:bg-gray-800 dark:hover:bg-gray-100 transition-colors"
            >
              Apply Changes
            </button>
            <button
              onClick={onReject}
              className="px-5 py-2.5 bg-white dark:bg-black text-black dark:text-white border border-gray-300 dark:border-gray-700 rounded-lg font-medium hover:bg-gray-50 dark:hover:bg-gray-900 transition-colors"
            >
              Reject
            </button>
          </div>
        </div>
        
        <div className="flex-1 overflow-hidden">
          <DiffEditor
            original={originalCode}
            modified={modifiedCode}
            language={language}
            theme={theme === 'dark' ? 'vs-dark' : 'vs-light'}
            options={{
              readOnly: true,
              renderSideBySide: true,
              enableSplitViewResizing: true,
              renderOverviewRuler: false,
              minimap: { enabled: false },
              scrollBeyondLastLine: false,
              fontSize: 14,
              lineNumbers: 'on',
              wordWrap: 'on',
              automaticLayout: true,
              padding: { top: 16, bottom: 16 },
            }}
          />
        </div>
      </div>
    </div>
  );
}