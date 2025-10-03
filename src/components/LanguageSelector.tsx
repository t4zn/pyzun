'use client';

'use client';

import { useState, useRef, useEffect } from 'react';

interface LanguageSelectorProps {
  language: string;
  onChange: (language: string) => void;
}

const languages = [
  { value: 'assembly', label: 'Assembly', id: 45 },
  { value: 'bash', label: 'Bash', id: 46 },
  { value: 'basic', label: 'Basic', id: 47 },
  { value: 'c', label: 'C', id: 50 },
  { value: 'cpp', label: 'C++', id: 54 },
  { value: 'csharp', label: 'C#', id: 51 },
  { value: 'clojure', label: 'Clojure', id: 86 },
  { value: 'cobol', label: 'COBOL', id: 77 },
  { value: 'd', label: 'D', id: 56 },
  { value: 'elixir', label: 'Elixir', id: 57 },
  { value: 'erlang', label: 'Erlang', id: 58 },
  { value: 'fortran', label: 'Fortran', id: 59 },
  { value: 'go', label: 'Go', id: 60 },
  { value: 'haskell', label: 'Haskell', id: 61 },
  { value: 'java', label: 'Java', id: 62 },
  { value: 'javascript', label: 'JavaScript', id: 63 },
  { value: 'kotlin', label: 'Kotlin', id: 78 },
  { value: 'lisp', label: 'Lisp', id: 55 },
  { value: 'lua', label: 'Lua', id: 64 },
  { value: 'objective_c', label: 'Objective-C', id: 79 },
  { value: 'ocaml', label: 'OCaml', id: 65 },
  { value: 'octave', label: 'Octave', id: 66 },
  { value: 'pascal', label: 'Pascal', id: 67 },
  { value: 'perl', label: 'Perl', id: 85 },
  { value: 'php', label: 'PHP', id: 68 },
  { value: 'prolog', label: 'Prolog', id: 69 },
  { value: 'python', label: 'Python', id: 71 },
  { value: 'r', label: 'R', id: 80 },
  { value: 'ruby', label: 'Ruby', id: 72 },
  { value: 'rust', label: 'Rust', id: 73 },
  { value: 'scala', label: 'Scala', id: 81 },
  { value: 'sql', label: 'SQL', id: 82 },
  { value: 'swift', label: 'Swift', id: 83 },
  { value: 'typescript', label: 'TypeScript', id: 74 },
  { value: 'visual_basic', label: 'Visual Basic', id: 84 }
];

export default function LanguageSelector({ language, onChange }: LanguageSelectorProps) {
  const [isOpen, setIsOpen] = useState(false);
  const [searchTerm, setSearchTerm] = useState('');
  const dropdownRef = useRef<HTMLDivElement>(null);
  const searchInputRef = useRef<HTMLInputElement>(null);

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
        <span>{selectedLanguage?.label || 'Select Language'}</span>
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
              className="w-full px-2 py-1 bg-transparent outline-none font-mono text-sm"
              style={{ 
                color: 'var(--foreground)',
                backgroundColor: 'transparent'
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
                  className="w-full px-4 py-2 text-left hover:opacity-70 transition-all duration-150 font-medium text-sm hover:translate-x-1"
                  style={{
                    backgroundColor: lang.value === language ? 'var(--foreground)' : 'var(--background)',
                    color: lang.value === language ? 'var(--background)' : 'var(--foreground)'
                  }}
                >
                  {lang.label}
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