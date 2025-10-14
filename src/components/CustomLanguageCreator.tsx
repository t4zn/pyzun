'use client';

import { useState } from 'react';
import { useTheme } from './ThemeProvider';
import CustomLanguageService from '../services/customLanguageService';
import { languages } from './LanguageSelector';

interface CustomLanguage {
  name: string;
  extension: string;
  keywords: Record<string, string>;
}

interface CustomLanguageCreatorProps {
  isOpen: boolean;
  onClose: () => void;
  onSave: (language: CustomLanguage) => void;
}

const pythonKeywords = [
  'print', 'if', 'else', 'elif', 'for', 'while', 'def', 'class',
  'import', 'from', 'return', 'try', 'except', 'with', 'as',
  'and', 'or', 'not', 'in', 'is', 'True', 'False', 'None'
];

export default function CustomLanguageCreator({ isOpen, onClose, onSave }: CustomLanguageCreatorProps) {
  const { theme } = useTheme();
  const [languageName, setLanguageName] = useState('');
  const [extension, setExtension] = useState('');
  const [keywordTranslations, setKeywordTranslations] = useState<Record<string, string>>({});
  const [nameError, setNameError] = useState('');
  const [extensionError, setExtensionError] = useState('');

  const handleKeywordChange = (pythonKeyword: string, customTranslation: string) => {
    if (customTranslation.trim()) {
      setKeywordTranslations(prev => ({
        ...prev,
        [customTranslation.trim()]: pythonKeyword
      }));
    } else {
      // Remove translation if empty
      setKeywordTranslations(prev => {
        const updated = { ...prev };
        // Find and remove the entry for this python keyword
        Object.keys(updated).forEach(key => {
          if (updated[key] === pythonKeyword) {
            delete updated[key];
          }
        });
        return updated;
      });
    }
  };

  const getCurrentTranslation = (pythonKeyword: string): string => {
    const entry = Object.entries(keywordTranslations).find(([_, value]) => value === pythonKeyword);
    return entry ? entry[0] : '';
  };

  const validateName = (name: string) => {
    if (!name.trim()) {
      setNameError('');
      return true;
    }

    // Check against built-in languages
    const builtInLanguage = languages.find(lang => 
      lang.label.toLowerCase() === name.trim().toLowerCase()
    );
    if (builtInLanguage) {
      setNameError('This name is already used by a built-in language');
      return false;
    }

    // Check against existing custom languages
    const customLanguages = CustomLanguageService.getLanguages();
    const existingCustom = Object.values(customLanguages).find(lang => 
      lang.name.toLowerCase() === name.trim().toLowerCase()
    );
    if (existingCustom) {
      setNameError('This name is already used by another custom language');
      return false;
    }

    setNameError('');
    return true;
  };

  const validateExtension = (ext: string) => {
    if (!ext.trim()) {
      setExtensionError('');
      return true;
    }

    // Get built-in extensions
    const builtInExtensions = [
      'asm', 'sh', 'bas', 'c', 'cpp', 'cs', 'clj', 'cob', 'd', 'ex', 'erl', 
      'f90', 'go', 'hs', 'java', 'js', 'kt', 'lisp', 'lua', 'm', 'ml', 'pas', 
      'pl', 'php', 'py', 'r', 'rb', 'rs', 'scala', 'sql', 'swift', 'ts', 'vb', 'ved'
    ];

    if (builtInExtensions.includes(ext.trim().toLowerCase())) {
      setExtensionError('This extension is already used by a built-in language');
      return false;
    }

    // Check against existing custom languages
    const customLanguages = CustomLanguageService.getLanguages();
    const existingCustom = Object.values(customLanguages).find(lang => 
      lang.extension.toLowerCase() === ext.trim().toLowerCase()
    );
    if (existingCustom) {
      setExtensionError('This extension is already used by another custom language');
      return false;
    }

    setExtensionError('');
    return true;
  };

  const handleSave = () => {
    const isNameValid = validateName(languageName);
    const isExtensionValid = validateExtension(extension);
    
    if (languageName.trim() && extension.trim() && isNameValid && isExtensionValid) {
      onSave({
        name: languageName.trim(),
        extension: extension.trim(),
        keywords: keywordTranslations
      });
      handleReset();
      onClose();
    }
  };

  const handleReset = () => {
    setLanguageName('');
    setExtension('');
    setKeywordTranslations({});
    setNameError('');
    setExtensionError('');
  };

  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 bg-black/30 backdrop-blur-sm flex items-center justify-center z-50 p-4" style={{ backdropFilter: 'blur(4px)' }}>
      <div
        className="w-full max-w-md max-h-[80vh] overflow-y-auto rounded-lg shadow-xl border"
        style={{
          backgroundColor: 'var(--background)',
          color: 'var(--foreground)',
          borderColor: theme === 'dark' ? '#374151' : '#e5e7eb'
        }}
      >
        <div className="p-4">
          <div className="flex items-center justify-between mb-4">
            <h2 className="text-lg font-medium">Create Language</h2>
            <button
              onClick={onClose}
              className="p-1 hover:opacity-70 transition-opacity"
              style={{ color: 'var(--foreground)' }}
            >
              <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                <line x1="18" y1="6" x2="6" y2="18" />
                <line x1="6" y1="6" x2="18" y2="18" />
              </svg>
            </button>
          </div>

          <div className="space-y-4">
            {/* Language Name */}
            <div>
              <label className="block text-xs font-medium mb-1 opacity-70">Language Name</label>
              <input
                type="text"
                value={languageName}
                onChange={(e) => {
                  setLanguageName(e.target.value);
                  validateName(e.target.value);
                }}
                placeholder="e.g., MyLang, Español"
                className="w-full px-2 py-1.5 text-sm border rounded bg-transparent outline-none focus:ring-1 focus:ring-blue-500"
                style={{
                  borderColor: nameError ? '#ef4444' : (theme === 'dark' ? '#374151' : '#d1d5db'),
                  color: 'var(--foreground)'
                }}
              />
              {nameError && (
                <p className="text-xs text-red-500 mt-1">{nameError}</p>
              )}
            </div>

            {/* File Extension */}
            <div>
              <label className="block text-xs font-medium mb-1 opacity-70">File Extension</label>
              <input
                type="text"
                value={extension}
                onChange={(e) => {
                  const cleanExt = e.target.value.replace(/[^a-zA-Z0-9]/g, '');
                  setExtension(cleanExt);
                  validateExtension(cleanExt);
                }}
                placeholder="e.g., mylang, esp"
                className="w-full px-2 py-1.5 text-sm border rounded bg-transparent outline-none focus:ring-1 focus:ring-blue-500"
                style={{
                  borderColor: extensionError ? '#ef4444' : (theme === 'dark' ? '#374151' : '#d1d5db'),
                  color: 'var(--foreground)'
                }}
              />
              {extensionError && (
                <p className="text-xs text-red-500 mt-1">{extensionError}</p>
              )}
            </div>

            {/* Keywords Mapping */}
            <div>
              <label className="block text-xs font-medium mb-1 opacity-70">Custom Keywords</label>
              <p className="text-xs opacity-60 mb-2">
                Create your own keywords for common programming concepts. Leave blank to use standard syntax.
              </p>

              {/* Keywords Grid */}
              <div className="border rounded max-h-40 overflow-y-auto" style={{ borderColor: theme === 'dark' ? '#374151' : '#d1d5db' }}>
                {pythonKeywords.map((pythonKeyword) => (
                  <div key={pythonKeyword} className="flex items-center gap-3 p-3 border-b last:border-b-0" style={{ borderColor: theme === 'dark' ? '#374151' : '#e5e7eb' }}>
                    {/* Standard Keyword (Locked) */}
                    <div
                      className="flex-1 px-3 py-2 text-sm rounded font-mono"
                      style={{
                        backgroundColor: theme === 'dark' ? '#1f2937' : '#f3f4f6',
                        color: 'var(--foreground)'
                      }}
                    >
                      {pythonKeyword}
                    </div>

                    {/* Custom Keyword (Editable) */}
                    <input
                      type="text"
                      value={getCurrentTranslation(pythonKeyword)}
                      onChange={(e) => handleKeywordChange(pythonKeyword, e.target.value)}
                      placeholder={`Your keyword`}
                      className="flex-1 px-3 py-2 text-sm rounded font-mono outline-none focus:ring-1 focus:ring-blue-500"
                      style={{
                        backgroundColor: theme === 'dark' ? '#1f2937' : '#f3f4f6',
                        color: 'var(--foreground)'
                      }}
                    />
                  </div>
                ))}
              </div>
            </div>

            {/* Actions */}
            <div className="flex gap-2 pt-2">
              <button
                onClick={handleSave}
                disabled={!languageName.trim() || !extension.trim() || !!nameError || !!extensionError}
                className="flex-1 py-1.5 px-3 text-sm bg-blue-500 text-white rounded hover:bg-blue-600 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
              >
                Create
              </button>
              <button
                onClick={handleReset}
                className="px-3 py-1.5 text-sm border rounded hover:opacity-70 transition-opacity"
                style={{
                  borderColor: theme === 'dark' ? '#374151' : '#d1d5db',
                  color: 'var(--foreground)'
                }}
              >
                Reset
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}