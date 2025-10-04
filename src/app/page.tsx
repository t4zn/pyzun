'use client';

import { useState } from 'react';
import CodeEditor from '../components/Editor';
import Output from '../components/Output';
import LanguageSelector from '../components/LanguageSelector';
import ResizablePanels from '../components/ResizablePanels';

import { useJudge0 } from '../hooks/useJudge0';
import { useAICodeFix } from '../hooks/useAICodeFix';
import { useTheme } from '../components/ThemeProvider';
import InfoOverlay from '../components/info-overlay';

const defaultCode: Record<string, string> = {
  assembly: `section .data
    hello db 'Hello, World!', 10, 0
    hello_len equ $ - hello
    
section .text
    global _start
    
_start:
    ; Print hello
    mov eax, 4
    mov ebx, 1
    mov ecx, hello
    mov edx, hello_len
    int 0x80
    
    ; Exit
    mov eax, 1
    mov ebx, 0
    int 0x80`,

  bash: `#!/bin/bash
echo "Hello, World!"
echo "Welcome to Pyzun!"`,

  basic: `print "Hello, World!"
print "Welcome to Pyzun!"`,

  c: `#include <stdio.h>

int main() {
    printf("Hello, World!\\n");
    printf("Welcome to Pyzun!\\n");
    return 0;
}`,

  cpp: `#include <iostream>
using namespace std;

int main() {
    cout << "Hello, World!" << endl;
    cout << "Welcome to Pyzun!" << endl;
    return 0;
}`,

  csharp: `using System;

class Program {
    static void Main() {
        Console.WriteLine("Hello, World!");
        Console.WriteLine("Welcome to Pyzun!");
    }
}`,

  clojure: `(println "Hello, World!")
(println "Welcome to Pyzun!")`,

  cobol: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       
       PROCEDURE DIVISION.
       DISPLAY "Hello, World!".
       DISPLAY "Welcome to Pyzun!".
       STOP RUN.
`,

  d: `import std.stdio;

void main() {
    writeln("Hello, World!");
    writeln("Welcome to Pyzun!");
}`,

  elixir: `IO.puts("Hello, World!")
IO.puts("Welcome to Pyzun!")`,

  erlang: `-module(main).
-export([start/0]).

start() ->
    io:format("Hello, World!~n"),
    io:format("Welcome to Pyzun!~n").`,

  fortran: `program hello
    print *, 'Hello, World!'
    print *, 'Welcome to Pyzun!'
end program hello`,

  go: `package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
    fmt.Println("Welcome to Pyzun!")
}`,

  haskell: `main = do
    putStrLn "Hello, World!"
    putStrLn "Welcome to Pyzun!"`,

  java: `public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
        System.out.println("Welcome to Pyzun!");
    }
}`,

  javascript: `console.log("Hello, World!");
console.log("Welcome to Pyzun!");`,

  kotlin: `fun main() {
    println("Hello, World!")
    println("Welcome to Pyzun!")
}`,

  lisp: `(write-line "Hello, World!")
(write-line "Welcome to Pyzun!")`,

  lua: `print("Hello, World!")
print("Welcome to Pyzun!")`,

  objective_c: `#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSLog(@"Hello, World!");
        NSLog(@"Welcome to Pyzun!");
    }
    return 0;
}`,

  ocaml: `print_endline "Hello, World!";;
print_endline "Welcome to Pyzun!";;`,

  octave: `disp('Hello, World!')
disp('Welcome to Pyzun!')`,

  pascal: `program Hello;
begin
    writeln('Hello, World!');
    writeln('Welcome to Pyzun!');
end.`,

  perl: `print "Hello, World!\\n";
print "Welcome to Pyzun!\\n";`,

  php: `<?php
echo "Hello, World!\\n";
echo "Welcome to Pyzun!\\n";
?>`,

  prolog: `:- initialization(main).

main :-
    write('Hello, World!'), nl,
    write('Welcome to Pyzun!'), nl,
    halt.`,

  python: `print("Hello, World!")
print("Welcome to Pyzun!")`,

  r: `cat("Hello, World!\\n")
cat("Welcome to Pyzun!\\n")`,

  ruby: `puts "Hello, World!"
puts "Welcome to Pyzun!"`,

  rust: `fn main() {
    println!("Hello, World!");
    println!("Welcome to Pyzun!");
}`,

  scala: `object Main {
    def main(args: Array[String]): Unit = {
        println("Hello, World!")
        println("Welcome to Pyzun!")
    }
}`,

  sql: `SELECT 'Hello, World!' AS greeting;
SELECT 'Welcome to Pyzun!' AS message;`,

  swift: `print("Hello, World!")
print("Welcome to Pyzun!")`,

  typescript: `console.log("Hello, World!");
console.log("Welcome to Pyzun!");`,

  visual_basic: `Imports System

Module Program
    Sub Main()
        Console.WriteLine("Hello, World!")
        Console.WriteLine("Welcome to Pyzun!")
    End Sub
End Module`
};

export default function Home() {
  const [language, setLanguage] = useState('python');
  const [code, setCode] = useState(defaultCode.python);
  const [stdin] = useState('');
  const [output, setOutput] = useState('');
  const [error, setError] = useState('');
  const [executionStats, setExecutionStats] = useState<{
    time: number | null;
    memory: number | null;
  }>({ time: null, memory: null });
  const { executeCode, isLoading } = useJudge0();
  const { theme, toggleTheme } = useTheme();
  const { isLoading: isFixingCode, suggestedCode, showDiff, fixCode, applyFix, rejectFix } = useAICodeFix();

  const handleLanguageChange = (newLanguage: string) => {
    setLanguage(newLanguage);
    setCode(defaultCode[newLanguage] || '// Write your code here');
    setOutput('');
    setError('');
    setExecutionStats({ time: null, memory: null });
  };

  const handleRunCode = async () => {
    setOutput('');
    setError('');
    setExecutionStats({ time: null, memory: null });
    
    const result = await executeCode(code, language, stdin);
    setOutput(result.output);
    setError(result.error);
    setExecutionStats({
      time: result.executionTime,
      memory: result.memoryUsed
    });
  };

  const handleAIFix = async () => {
    if (error && code) {
      await fixCode(code, error, language);
    }
  };

  const handleApplyFix = () => {
    applyFix((fixedCode: string) => {
      setCode(fixedCode);
      setError('');
      setOutput('');
    });
  };

  const handleRejectFix = () => {
    rejectFix((code: string) => {
      setCode(code);
    });
  };

  const getFileExtension = (lang: string): string => {
    const extensionMap: Record<string, string> = {
      assembly: 'asm',
      bash: 'sh',
      basic: 'bas',
      c: 'c',
      cpp: 'cpp',
      csharp: 'cs',
      clojure: 'clj',
      cobol: 'cob',
      d: 'd',
      elixir: 'ex',
      erlang: 'erl',
      fortran: 'f90',
      go: 'go',
      haskell: 'hs',
      java: 'java',
      javascript: 'js',
      kotlin: 'kt',
      lisp: 'lisp',
      lua: 'lua',
      objective_c: 'm',
      ocaml: 'ml',
      octave: 'm',
      pascal: 'pas',
      perl: 'pl',
      php: 'php',
      prolog: 'pl',
      python: 'py',
      r: 'r',
      ruby: 'rb',
      rust: 'rs',
      scala: 'scala',
      sql: 'sql',
      swift: 'swift',
      typescript: 'ts',
      visual_basic: 'vb'
    };
    return extensionMap[lang] || 'txt';
  };

  const handleDownloadCode = () => {
    const extension = getFileExtension(language);
    const filename = `code.${extension}`;
    
    const blob = new Blob([code], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    
    const link = document.createElement('a');
    link.href = url;
    link.download = filename;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    
    URL.revokeObjectURL(url);
  };

  return (
    <div className="min-h-screen bg-white dark:bg-black text-black dark:text-white transition-colors" style={{ backgroundColor: 'var(--background)', color: 'var(--foreground)' }}>
      {/* Header */}
      <header className="px-8 py-6" style={{ backgroundColor: 'var(--background)' }}>
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-3">
            <img src="/pyzun.svg" alt="Pyzun" className="w-8 h-8" />
            <h1 className="text-3xl font-medium tracking-wide" style={{ color: 'var(--foreground)', fontFamily: 'var(--font-league-spartan), sans-serif' }}>Pyzun</h1>
          </div>
          <div className="flex items-center gap-6">
            <LanguageSelector language={language} onChange={handleLanguageChange} />
            <button
              onClick={toggleTheme}
              className="p-3 transition-all duration-200"
              style={{ 
                color: 'var(--foreground)'
              }}
              onMouseEnter={(e) => {
                e.currentTarget.style.opacity = '0.7';
              }}
              onMouseLeave={(e) => {
                e.currentTarget.style.opacity = '1';
              }}
              aria-label="Toggle theme"
            >
              {theme === 'light' ? (
                <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                  <path d="M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z" />
                </svg>
              ) : (
                <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                  <circle cx="12" cy="12" r="5" />
                  <path d="M12 1v2M12 21v2M4.22 4.22l1.42 1.42M18.36 18.36l1.42 1.42M1 12h2M21 12h2M4.22 19.78l1.42-1.42M18.36 5.64l1.42-1.42" />
                </svg>
              )}
            </button>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <div className="h-[calc(100vh-89px)]">
        <ResizablePanels
          defaultLeftWidth={50}
          minLeftWidth={25}
          maxLeftWidth={75}
          leftPanel={
            <div className="h-full flex flex-col p-8 animate-slide-in" style={{ backgroundColor: 'var(--background)' }}>
              <div className="flex items-center justify-between mb-6">
                <h2 className="text-xl font-light" style={{ color: 'var(--foreground)' }}>Editor</h2>
                <div className="flex items-center gap-2">
                  <button
                    onClick={() => window.open(window.location.href, '_blank')}
                    className="p-3 transition-all duration-200"
                    style={{
                      color: 'var(--foreground)'
                    }}
                    onMouseEnter={(e) => {
                      e.currentTarget.style.opacity = '0.7';
                    }}
                    onMouseLeave={(e) => {
                      e.currentTarget.style.opacity = '1';
                    }}
                    aria-label="New file"
                  >
                    <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" className="transition-transform hover:scale-110">
                      <line x1="12" y1="5" x2="12" y2="19" />
                      <line x1="5" y1="12" x2="19" y2="12" />
                    </svg>
                  </button>
                  <button
                    onClick={handleDownloadCode}
                    className="p-3 transition-all duration-200"
                    style={{
                      color: 'var(--foreground)'
                    }}
                    onMouseEnter={(e) => {
                      e.currentTarget.style.opacity = '0.7';
                    }}
                    onMouseLeave={(e) => {
                      e.currentTarget.style.opacity = '1';
                    }}
                    aria-label="Download code"
                  >
                    <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" className="transition-transform hover:scale-110">
                      <path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4" />
                      <polyline points="7,10 12,15 17,10" />
                      <line x1="12" y1="15" x2="12" y2="3" />
                    </svg>
                  </button>
                  <button
                    onClick={handleRunCode}
                    disabled={isLoading}
                    className="p-3 transition-all duration-200 disabled:cursor-not-allowed disabled:opacity-50"
                    style={{
                      color: 'var(--foreground)'
                    }}
                    onMouseEnter={(e) => {
                      if (!isLoading) {
                        e.currentTarget.style.opacity = '0.7';
                      }
                    }}
                    onMouseLeave={(e) => {
                      if (!isLoading) {
                        e.currentTarget.style.opacity = '1';
                      }
                    }}
                    aria-label={isLoading ? 'Running code...' : 'Run code'}
                  >
                    {isLoading ? (
                      <div className="flex items-center justify-center w-5 h-5">
                        <span className="inline-flex">
                          <span className="w-1 h-1 bg-current rounded-full animate-pulse" style={{ animationDelay: '0ms' }}></span>
                          <span className="w-1 h-1 bg-current rounded-full animate-pulse mx-0.5" style={{ animationDelay: '200ms' }}></span>
                          <span className="w-1 h-1 bg-current rounded-full animate-pulse" style={{ animationDelay: '400ms' }}></span>
                        </span>
                      </div>
                    ) : (
                      <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" className="transition-transform hover:scale-110">
                        <polygon points="5,3 19,12 5,21" />
                      </svg>
                    )}
                  </button>
                </div>
              </div>
              <div className="flex-1 min-h-[300px]">
                <CodeEditor
                  value={code}
                  onChange={(value) => setCode(value || '')}
                  language={language}
                  suggestedCode={suggestedCode || undefined}
                  showDiff={showDiff}
                  onApplyDiff={handleApplyFix}
                  onRejectDiff={handleRejectFix}
                />
              </div>
            </div>
          }
          rightPanel={
            <div className="h-full flex flex-col p-8 animate-fade-in" style={{ backgroundColor: 'var(--background)' }}>
              <div className="flex-1 min-h-[300px]">
                <Output 
                  output={output} 
                  error={error} 
                  isLoading={isLoading}
                  executionTime={executionStats.time}
                  memoryUsed={executionStats.memory}
                  onAIFix={handleAIFix}
                  isFixingCode={isFixingCode}
                />
              </div>
            </div>
          }
        />
      </div>

      {/* Info Overlay */}
      <InfoOverlay />
    </div>
  );
}
