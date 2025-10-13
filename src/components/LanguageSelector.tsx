'use client';

'use client';

import { useState, useRef, useEffect } from 'react';
import { useTheme } from './ThemeProvider';

interface LanguageSelectorProps {
  language: string;
  onChange: (language: string) => void;
}

const languages = [
  { value: 'sanskrit', label: 'Sanskrit', id: 85, icon: 'om-text', isNew: true },
  { value: 'assembly', label: 'Assembly', id: 45, icon: 'devicon-cplusplus-plain' },
  { value: 'bash', label: 'Bash', id: 46, icon: 'devicon-bash-plain' },
  { value: 'basic', label: 'Basic', id: 47, icon: 'devicon-bootstrap-plain' },
  { value: 'c', label: 'C', id: 50, icon: 'devicon-c-plain' },
  { value: 'cpp', label: 'C++', id: 54, icon: 'devicon-cplusplus-plain' },
  { value: 'csharp', label: 'C#', id: 51, icon: 'devicon-csharp-plain' },
  { value: 'clojure', label: 'Clojure', id: 86, icon: 'devicon-clojure-plain' },
  { value: 'cobol', label: 'COBOL', id: 77, icon: 'devicon-cobol-plain' },
  { value: 'd', label: 'D', id: 56, icon: 'devicon-denojs-plain' },
  { value: 'elixir', label: 'Elixir', id: 57, icon: 'devicon-elixir-plain' },
  { value: 'erlang', label: 'Erlang', id: 58, icon: 'devicon-erlang-plain' },
  { value: 'fortran', label: 'Fortran', id: 59, icon: 'devicon-fortran-original' },
  { value: 'go', label: 'Go', id: 60, icon: 'devicon-go-plain' },
  { value: 'haskell', label: 'Haskell', id: 61, icon: 'devicon-haskell-plain' },
  { value: 'java', label: 'Java', id: 62, icon: 'devicon-java-plain' },
  { value: 'javascript', label: 'JavaScript', id: 63, icon: 'devicon-javascript-plain' },
  { value: 'kotlin', label: 'Kotlin', id: 78, icon: 'devicon-kotlin-plain' },
  { value: 'lisp', label: 'Lisp', id: 55, icon: 'lisp-png' },
  { value: 'lua', label: 'Lua', id: 64, icon: 'devicon-lua-plain' },
  { value: 'objective_c', label: 'Objective-C', id: 79, icon: 'devicon-objectivec-plain' },
  { value: 'ocaml', label: 'OCaml', id: 65, icon: 'devicon-ocaml-plain' },
  { value: 'octave', label: 'Octave', id: 66, icon: 'devicon-matlab-plain' },
  { value: 'pascal', label: 'Pascal', id: 67, icon: 'devicon-delphi-plain' },
  { value: 'perl', label: 'Perl', id: 85, icon: 'devicon-perl-plain' },
  { value: 'php', label: 'PHP', id: 68, icon: 'devicon-php-plain' },
  { value: 'prolog', label: 'Prolog', id: 69, icon: 'devicon-prolog-plain' },
  { value: 'python', label: 'Python', id: 71, icon: 'devicon-python-plain' },
  { value: 'r', label: 'R', id: 80, icon: 'devicon-r-plain' },
  { value: 'ruby', label: 'Ruby', id: 72, icon: 'devicon-ruby-plain' },
  { value: 'rust', label: 'Rust', id: 73, icon: 'devicon-rust-plain' },
  { value: 'scala', label: 'Scala', id: 81, icon: 'devicon-scala-plain' },
  { value: 'sql', label: 'SQL', id: 82, icon: 'devicon-mysql-plain' },
  { value: 'swift', label: 'Swift', id: 83, icon: 'devicon-swift-plain' },
  { value: 'typescript', label: 'TypeScript', id: 74, icon: 'devicon-typescript-plain' },
  { value: 'visual_basic', label: 'Visual Basic', id: 84, icon: 'devicon-visualbasic-plain' }
];

export default function LanguageSelector({ language, onChange }: LanguageSelectorProps) {
  const [isOpen, setIsOpen] = useState(false);
  const [searchTerm, setSearchTerm] = useState('');
  const dropdownRef = useRef<HTMLDivElement>(null);
  const searchInputRef = useRef<HTMLInputElement>(null);
  const { theme } = useTheme();

  const filteredLanguages = languages.filter(lang =>
    lang.label.toLowerCase().includes(searchTerm.toLowerCase())
  );

  const selectedLanguage = languages.find(lang => lang.value === language);

  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (dropdownRef.current && !dropdownRef.current.contains(event.target as Node)) {
        setIsOpen(false);
        setSearchTerm('');
      }
    };

    document.addEventListener('mousedown', handleClickOutside);
    return () => document.removeEventListener('mousedown', handleClickOutside);
  }, []);

  useEffect(() => {
    if (isOpen && searchInputRef.current) {
      searchInputRef.current.focus();
    }
  }, [isOpen]);

  const handleSelect = (langValue: string) => {
    onChange(langValue);
    setIsOpen(false);
    setSearchTerm('');
  };

  return (
    <div className="relative" ref={dropdownRef}>
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="px-4 py-3 font-medium uppercase tracking-wider text-sm flex items-center justify-between min-w-[200px] transition-all duration-200 hover:opacity-80"
        style={{
          backgroundColor: 'var(--background)',
          color: 'var(--foreground)'
        }}
      >
        <div className="flex items-center gap-2">
          {selectedLanguage && (
            isOmIcon(selectedLanguage.icon) ? (
              <span style={{ fontSize: '16px', fontWeight: 'bold' }}>ॐ</span>
            ) : isLispIcon(selectedLanguage.icon) ? (
              <img src="/lisp.png" alt="Lisp" style={{ width: '16px', height: '16px' }} />
            ) : (
              <i className={selectedLanguage.icon} style={{ fontSize: '16px' }}></i>
            )
          )}
          <span>{selectedLanguage?.label || 'Select Language'}</span>
        </div>
        <svg 
          width="12" 
          height="12" 
          viewBox="0 0 24 24" 
          fill="none" 
          stroke="currentColor" 
          strokeWidth="2"
          className={`transition-transform duration-200 ${isOpen ? 'rotate-180' : ''}`}
        >
          <polyline points="6,9 12,15 18,9"></polyline>
        </svg>
      </button>

      {isOpen && (
        <div 
          className="absolute top-full left-0 right-0 max-h-80 overflow-hidden z-50 animate-fade-in"
          style={{
            backgroundColor: 'var(--background)'
          }}
        >
          {/* Search Input */}
          <div className="p-2">
            <input
              ref={searchInputRef}
              type="text"
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              placeholder="Search languages..."
              className="w-full px-2 py-1 bg-transparent outline-none font-mono text-sm rounded border border-gray-200 dark:border-gray-700"
              style={{ 
                color: 'var(--foreground)',
                backgroundColor: 'var(--background)',
                boxShadow: theme === 'dark' 
                  ? '0 0 0 1px rgba(255, 255, 255, 0.1), 0 4px 6px -1px rgba(255, 255, 255, 0.1), 0 2px 4px -1px rgba(255, 255, 255, 0.06)'
                  : '0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)'
              }}
            />
          </div>

          {/* Language List */}
          <div className="overflow-y-auto max-h-64">
            {filteredLanguages.length > 0 ? (
              filteredLanguages.map((lang) => (
                <button
                  key={lang.value}
                  onClick={() => handleSelect(lang.value)}
                  className="w-full px-4 py-2 text-left hover:opacity-70 transition-all duration-150 font-medium text-sm hover:translate-x-1 flex items-center gap-2 justify-between"
                  style={{
                    backgroundColor: lang.value === language ? 'var(--foreground)' : 'var(--background)',
                    color: lang.value === language ? 'var(--background)' : 'var(--foreground)'
                  }}
                >
                  <div className="flex items-center gap-2">
                    {isOmIcon(lang.icon) ? (
                      <span style={{ fontSize: '16px', fontWeight: 'bold' }}>ॐ</span>
                    ) : isLispIcon(lang.icon) ? (
                      <img src="/lisp.png" alt="Lisp" style={{ width: '16px', height: '16px' }} />
                    ) : (
                      <i className={lang.icon} style={{ fontSize: '16px' }}></i>
                    )}
                    <span>{lang.label}</span>
                  </div>
                  {lang.isNew && (
                    <span 
                      className="px-1 py-0.5 font-medium rounded opacity-70"
                      style={{
                        backgroundColor: 'var(--foreground)',
                        color: 'var(--background)',
                        fontSize: '9px'
                      }}
                    >
                      NEW
                    </span>
                  )}
                </button>
              ))
            ) : (
              <div className="px-4 py-2 text-sm opacity-50" style={{ color: 'var(--foreground)' }}>
                No languages found
              </div>
            )}
          </div>
        </div>
      )}
    </div>
  );
}

export { languages };

export const getLanguageIcon = (languageValue: string): string => {
  const lang = languages.find(l => l.value === languageValue);
  return lang?.icon || 'devicon-plain-wordmark';
};

export const isOmIcon = (icon: string): boolean => {
  return icon === 'om-text';
};

export const isLispIcon = (icon: string): boolean => {
  return icon === 'lisp-png';
};