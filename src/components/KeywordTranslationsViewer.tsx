'use client';

import { useTheme } from './ThemeProvider';
import CustomLanguageService, { CustomLanguage } from '../services/customLanguageService';

interface KeywordTranslationsViewerProps {
  isOpen: boolean;
  onClose: () => void;
  language: string;
}

export default function KeywordTranslationsViewer({ isOpen, onClose, language }: KeywordTranslationsViewerProps) {
  const { theme } = useTheme();

  if (!isOpen || !CustomLanguageService.isCustomLanguage(language)) {
    return null;
  }

  const customLang = CustomLanguageService.getCustomLanguage(language);
  if (!customLang) {
    return null;
  }

  const keywordEntries = Object.entries(customLang.keywords);

  return (
    <div className="fixed inset-0 bg-black/30 backdrop-blur-sm flex items-center justify-center z-50 p-4" style={{ backdropFilter: 'blur(4px)' }}>
      <div 
        className="w-full max-w-lg max-h-[80vh] overflow-y-auto rounded-lg shadow-xl border"
        style={{ 
          backgroundColor: 'var(--background)', 
          color: 'var(--foreground)',
          borderColor: theme === 'dark' ? '#374151' : '#e5e7eb'
        }}
      >
        <div className="p-4">
          <div className="flex items-center justify-between mb-4">
            <div className="flex items-center gap-2">
              <span style={{ fontSize: '16px', fontWeight: 'bold' }}>&lt;/&gt;</span>
              <h2 className="text-lg font-medium">{customLang.name} Keywords</h2>
            </div>
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

          {keywordEntries.length > 0 ? (
            <div className="space-y-3">
              <div className="text-sm opacity-70 mb-3" style={{ color: 'var(--foreground)' }}>
                {keywordEntries.length} custom keyword{keywordEntries.length !== 1 ? 's' : ''} defined
              </div>
              
              <div className="space-y-2">
                {keywordEntries.map(([customKeyword, pythonKeyword]) => (
                  <div key={customKeyword} className="flex items-center gap-3 p-3 rounded border" style={{ borderColor: theme === 'dark' ? '#374151' : '#e5e7eb' }}>
                    <div className="flex-1">
                      <div className="text-xs opacity-60 mb-1" style={{ color: 'var(--foreground)' }}>Your keyword</div>
                      <span 
                        className="px-3 py-2 rounded font-mono font-semibold text-sm block"
                        style={{
                          backgroundColor: theme === 'dark' ? '#1f2937' : '#f3f4f6',
                          color: theme === 'dark' ? '#569cd6' : '#0000ff'
                        }}
                      >
                        {customKeyword}
                      </span>
                    </div>
                    
                    <div className="flex-shrink-0 opacity-40">
                      <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                        <line x1="5" y1="12" x2="19" y2="12" />
                        <polyline points="12,5 19,12 12,19" />
                      </svg>
                    </div>
                    
                    <div className="flex-1">
                      <div className="text-xs opacity-60 mb-1" style={{ color: 'var(--foreground)' }}>Standard equivalent</div>
                      <span 
                        className="px-3 py-2 rounded font-mono text-sm block"
                        style={{
                          backgroundColor: theme === 'dark' ? '#1f2937' : '#f3f4f6',
                          color: 'var(--foreground)'
                        }}
                      >
                        {pythonKeyword}
                      </span>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          ) : (
            <div className="text-center py-8 opacity-60" style={{ color: 'var(--foreground)' }}>
              <span style={{ fontSize: '24px', fontWeight: 'bold' }}>&lt;/&gt;</span>
              <p className="mt-2 text-sm">No custom keywords defined</p>
              <p className="text-xs mt-1">This language uses standard syntax</p>
            </div>
          )}
        </div>
      </div>
    </div>
  );
}