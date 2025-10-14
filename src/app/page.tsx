'use client';

import { useState } from 'react';
import CodeEditor from '../components/Editor';
import Output from '../components/Output';
import LanguageSelector, { getLanguageIcon, isOmIcon, isLispIcon, isAssemblyIcon, isBasicIcon } from '../components/LanguageSelector';
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
End Module`,

  sanskrit: `वद("नमस्ते विश्व!");
वद("पायज़न में आपका स्वागत है!");`
};

interface FileTab {
  id: string;
  filename: string;
  language: string;
  code: string;
  isModified: boolean;
  output?: string;
  error?: string;
  executionStats?: {
    time: number | null;
    memory: number | null;
  };
}

export default function Home() {
  const getDefaultFilename = (lang: string) => {
    const defaults: Record<string, string> = {
      assembly: 'main.asm',
      bash: 'script.sh',
      basic: 'main.bas',
      c: 'main.c',
      cpp: 'main.cpp',
      csharp: 'Program.cs',
      clojure: 'main.clj',
      cobol: 'main.cob',
      d: 'main.d',
      elixir: 'main.ex',
      erlang: 'main.erl',
      fortran: 'main.f90',
      go: 'main.go',
      haskell: 'main.hs',
      java: 'Main.java',
      javascript: 'script.js',
      kotlin: 'Main.kt',
      lisp: 'main.lisp',
      lua: 'main.lua',
      objective_c: 'main.m',
      ocaml: 'main.ml',
      octave: 'main.m',
      pascal: 'main.pas',
      perl: 'script.pl',
      php: 'index.php',
      prolog: 'main.pl',
      python: 'main.py',
      r: 'script.r',
      ruby: 'main.rb',
      rust: 'main.rs',
      scala: 'Main.scala',
      sql: 'query.sql',
      swift: 'main.swift',
      typescript: 'script.ts',
      visual_basic: 'Main.vb',
      sanskrit: 'hello.ved'
    };
    return defaults[lang] || 'main.txt';
  };

  // Initialize with one default file
  const [files, setFiles] = useState<FileTab[]>([
    {
      id: '1',
      filename: getDefaultFilename('python'),
      language: 'python',
      code: defaultCode.python,
      isModified: false,
      output: '',
      error: '',
      executionStats: { time: null, memory: null }
    }
  ]);
  const [activeFileId, setActiveFileId] = useState('1');
  const [isEditingFilename, setIsEditingFilename] = useState(false);
  const [editingName, setEditingName] = useState('');
  const [stdin, setStdin] = useState('');

  // Get current active file
  const activeFile = files.find(f => f.id === activeFileId) || files[0];
  const language = activeFile?.language || 'python';
  const code = activeFile?.code || '';
  const filename = activeFile?.filename || 'main.py';
  const output = activeFile?.output || '';
  const error = activeFile?.error || '';
  const executionStats = activeFile?.executionStats || { time: null, memory: null };
  const { executeCode, isLoading: judge0Loading } = useJudge0();
  const [sanskritLoading, setSanskritLoading] = useState(false);

  const isLoading = language === 'sanskrit' ? sanskritLoading : judge0Loading;
  const { theme, toggleTheme } = useTheme();
  const { isLoading: isFixingCode, suggestedCode, showDiff, fixCode, applyFix, rejectFix } = useAICodeFix();

  const updateActiveFile = (updates: Partial<FileTab>) => {
    setFiles(prev => prev.map(file =>
      file.id === activeFileId
        ? { ...file, ...updates, isModified: true }
        : file
    ));
  };

  const handleLanguageChange = (newLanguage: string) => {
    const newFilename = getDefaultFilename(newLanguage);
    const newCode = defaultCode[newLanguage] || '// Write your code here';

    updateActiveFile({
      language: newLanguage,
      filename: newFilename,
      code: newCode,
      isModified: false,
      output: '',
      error: '',
      executionStats: { time: null, memory: null }
    });
  };

  const handleCodeChange = (newCode: string | undefined) => {
    updateActiveFile({ code: newCode || '' });
  };

  const createNewFile = () => {
    const newId = Date.now().toString();
    const currentLanguage = language; // Use the currently active file's language
    const newFile: FileTab = {
      id: newId,
      filename: getDefaultFilename(currentLanguage),
      language: currentLanguage,
      code: defaultCode[currentLanguage] || '// Write your code here',
      isModified: false,
      output: '',
      error: '',
      executionStats: { time: null, memory: null }
    };

    setFiles(prev => [...prev, newFile]);
    setActiveFileId(newId);
  };

  const closeFile = (fileId: string) => {
    if (files.length === 1) return; // Don't close the last file

    setFiles(prev => {
      const newFiles = prev.filter(f => f.id !== fileId);
      // If we're closing the active file, switch to another one
      if (fileId === activeFileId) {
        const currentIndex = prev.findIndex(f => f.id === fileId);
        const nextFile = prev[currentIndex + 1] || prev[currentIndex - 1] || newFiles[0];
        setActiveFileId(nextFile.id);
      }
      return newFiles;
    });
  };

  const switchToFile = (fileId: string) => {
    setActiveFileId(fileId);
  };

  const getFilenameWithoutExtension = (fullFilename: string): string => {
    const lastDotIndex = fullFilename.lastIndexOf('.');
    return lastDotIndex === -1 ? fullFilename : fullFilename.substring(0, lastDotIndex);
  };

  const handleStartEditing = () => {
    const nameWithoutExt = getFilenameWithoutExtension(filename);
    setEditingName(nameWithoutExt);
    setIsEditingFilename(true);
  };

  const handleFinishEditing = () => {
    if (editingName.trim()) {
      const currentExtension = getFileExtension(language);
      const newFilename = `${editingName.trim()}.${currentExtension}`;
      updateActiveFile({ filename: newFilename });
    }
    setIsEditingFilename(false);
    setEditingName('');
  };

  const handleEditingNameChange = (newName: string) => {
    // Only allow alphanumeric characters, hyphens, underscores, and spaces
    const sanitizedName = newName.replace(/[^a-zA-Z0-9\-_\s]/g, '');
    setEditingName(sanitizedName);
  };

  const executeSanskritCode = (code: string) => {
    const lines = code.split('\n');
    const output: string[] = [];
    const errors: string[] = [];

    for (let i = 0; i < lines.length; i++) {
      const trimmedLine = lines[i].trim();

      // Skip empty lines
      if (!trimmedLine) continue;

      // Check if line starts with वद
      if (trimmedLine.startsWith('वद(')) {
        // Validate proper syntax: वद("message");
        const validSyntax = /^वद\("([^"]*)"\);$/.test(trimmedLine);

        if (validSyntax) {
          // Extract content between quotes
          const match = trimmedLine.match(/वद\("([^"]*)"\);/);
          if (match && match[1]) {
            output.push(match[1]);
          }
        } else {
          // Invalid वद syntax
          errors.push(`Syntax error on line ${i + 1}`);
        }
      } else {
        // Any other syntax is not allowed
        errors.push(`Unsupported syntax on line ${i + 1}`);
      }
    }

    if (errors.length > 0) {
      return {
        output: '',
        error: 'Your code either has errors or uses different syntax.\n\nSubscribe to unlock full Sanskrit support and advance syntax!'
      };
    }

    return {
      output: output.join('\n'),
      error: ''
    };
  };

  const handleRunCode = async () => {
    // Clear current file's output
    updateActiveFile({
      output: '',
      error: '',
      executionStats: { time: null, memory: null }
    });

    if (language === 'sanskrit') {
      // Handle Sanskrit code execution locally with delay
      setSanskritLoading(true);
      const startTime = Date.now();

      // Simulate execution delay of 2-3 seconds
      await new Promise(resolve => setTimeout(resolve, 2500));

      const result = executeSanskritCode(code);
      const executionTime = (Date.now() - startTime) / 1000;

      updateActiveFile({
        output: result.output,
        error: result.error,
        executionStats: {
          time: executionTime,
          memory: 0
        }
      });

      setSanskritLoading(false);
    } else {
      // Use Judge0 for other languages
      const result = await executeCode(code, language, stdin);

      // Update current file's output
      updateActiveFile({
        output: result.output,
        error: result.error,
        executionStats: {
          time: result.executionTime,
          memory: result.memoryUsed
        }
      });
    }
  };

  const handleAIFix = async () => {
    if (error && code) {
      await fixCode(code, error, language);
    }
  };

  const handleApplyFix = () => {
    applyFix((fixedCode: string) => {
      updateActiveFile({
        code: fixedCode,
        error: '',
        output: ''
      });
    });
  };

  const handleRejectFix = () => {
    rejectFix((originalCode: string) => {
      updateActiveFile({ code: originalCode });
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
      visual_basic: 'vb',
      sanskrit: 'ved'
    };
    return extensionMap[lang] || 'txt';
  };

  const handleDownloadCode = () => {
    const blob = new Blob([code], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);

    const link = document.createElement('a');
    link.href = url;
    link.download = filename; // Use the current filename from state
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);

    URL.revokeObjectURL(url);
  };

  return (
    <div className="min-h-screen bg-white dark:bg-black text-black dark:text-white transition-colors" style={{ backgroundColor: 'var(--background)', color: 'var(--foreground)' }}>
      {/* Header */}
      <header className="px-4 sm:px-6 lg:px-8 py-4 sm:py-6" style={{ backgroundColor: 'var(--background)' }}>
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-2 sm:gap-3">
            <img src="/pyzun.svg" alt="Pyzun" className="w-6 h-6 sm:w-8 sm:h-8" />
            <h1 className="text-xl sm:text-2xl lg:text-3xl font-medium tracking-wide" style={{ color: 'var(--foreground)', fontFamily: 'var(--font-league-spartan), sans-serif' }}>Pyzun</h1>
          </div>
          <div className="flex items-center gap-2 sm:gap-4 lg:gap-6">
            <LanguageSelector language={language} onChange={handleLanguageChange} />
            <button
              onClick={toggleTheme}
              className="p-2 sm:p-3 transition-all duration-200"
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
                <svg width="14" height="14" className="sm:w-4 sm:h-4" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                  <path d="M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z" />
                </svg>
              ) : (
                <svg width="14" height="14" className="sm:w-4 sm:h-4" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                  <circle cx="12" cy="12" r="5" />
                  <path d="M12 1v2M12 21v2M4.22 4.22l1.42 1.42M18.36 18.36l1.42 1.42M1 12h2M21 12h2M4.22 19.78l1.42-1.42M18.36 5.64l1.42-1.42" />
                </svg>
              )}
            </button>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <div className="h-[calc(100vh-73px)] sm:h-[calc(100vh-89px)]">
        {/* Mobile Layout - Stack vertically */}
        <div className="lg:hidden h-full flex flex-col">
          {/* Editor Section */}
          <div className="flex-1 min-h-0 flex flex-col p-4 animate-slide-in" style={{ backgroundColor: 'var(--background)' }}>
            <div className="flex items-center justify-between mb-4">
              <h2 className="text-lg font-light" style={{ color: 'var(--foreground)' }}>Editor</h2>
              <div className="flex items-center gap-1">
                <button
                  onClick={createNewFile}
                  className="p-2 transition-all duration-200"
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
                  <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" className="transition-transform hover:scale-110">
                    <line x1="12" y1="5" x2="12" y2="19" />
                    <line x1="5" y1="12" x2="19" y2="12" />
                  </svg>
                </button>
                <button
                  onClick={handleDownloadCode}
                  className="p-2 transition-all duration-200"
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
                  <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" className="transition-transform hover:scale-110">
                    <path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4" />
                    <polyline points="7,10 12,15 17,10" />
                    <line x1="12" y1="15" x2="12" y2="3" />
                  </svg>
                </button>
                <button
                  onClick={handleRunCode}
                  disabled={isLoading}
                  className="p-2 transition-all duration-200 disabled:cursor-not-allowed disabled:opacity-50"
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
                    <div className="flex items-center justify-center w-4 h-4">
                      <span className="inline-flex">
                        <span className="w-1 h-1 bg-current rounded-full animate-pulse" style={{ animationDelay: '0ms' }}></span>
                        <span className="w-1 h-1 bg-current rounded-full animate-pulse mx-0.5" style={{ animationDelay: '200ms' }}></span>
                        <span className="w-1 h-1 bg-current rounded-full animate-pulse" style={{ animationDelay: '400ms' }}></span>
                      </span>
                    </div>
                  ) : (
                    <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" className="transition-transform hover:scale-110">
                      <polygon points="5,3 19,12 5,21" />
                    </svg>
                  )}
                </button>
              </div>
            </div>

            <div className="flex-1 min-h-0 flex flex-col">
              {/* Tabs */}
              <div className="flex items-center overflow-x-auto scrollbar-hide">
                {files.map((file) => (
                  <div
                    key={file.id}
                    className={`flex items-center gap-2 px-2 py-2 cursor-pointer group min-w-0 rounded-t-md transition-all duration-200 ${file.id === activeFileId
                      ? 'shadow-sm'
                      : 'hover:bg-gray-50 dark:hover:bg-gray-800'
                      }`}
                    style={{
                      backgroundColor: file.id === activeFileId && theme === 'dark' ? '#1e1e1e' : 'transparent',
                      boxShadow: file.id === activeFileId
                        ? '0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)'
                        : 'none'
                    }}
                    onClick={() => switchToFile(file.id)}
                  >
                    {/* File Icon */}
                    <div className="flex-shrink-0">
                      {isOmIcon(getLanguageIcon(file.language)) ? (
                        <span
                          style={{
                            fontSize: '12px',
                            fontWeight: 'bold',
                            color: file.id === activeFileId ? '#fbbf24' : 'currentColor'
                          }}
                        >
                          ॐ
                        </span>
                      ) : isLispIcon(getLanguageIcon(file.language)) ? (
                        <img
                          src="/lisp.png"
                          alt="Lisp"
                          style={{
                            width: '12px',
                            height: '12px',
                            filter: file.id === activeFileId ? 'sepia(1) saturate(3) hue-rotate(35deg)' : 'none'
                          }}
                        />
                      ) : isAssemblyIcon(getLanguageIcon(file.language)) ? (
                        <img
                          src={theme === 'dark' ? '/assemblydark.PNG' : '/assemblylight.PNG'}
                          alt="Assembly"
                          style={{
                            width: '14px',
                            height: '14px',
                            filter: file.id === activeFileId ? 'sepia(1) saturate(3) hue-rotate(35deg)' : 'none'
                          }}
                        />
                      ) : isBasicIcon(getLanguageIcon(file.language)) ? (
                        <img
                          src={theme === 'dark' ? '/basicdark.PNG' : '/basiclight.PNG'}
                          alt="Basic"
                          style={{
                            width: '10px',
                            height: '10px',
                            filter: file.id === activeFileId ? 'sepia(1) saturate(3) hue-rotate(35deg)' : 'none'
                          }}
                        />
                      ) : (
                        <i
                          className={getLanguageIcon(file.language)}
                          style={{
                            fontSize: '12px',
                            color: file.id === activeFileId ? '#fbbf24' : 'currentColor'
                          }}
                        ></i>
                      )}
                    </div>

                    {/* Filename */}
                    {isEditingFilename && file.id === activeFileId ? (
                      <div className="flex items-center min-w-0">
                        <input
                          type="text"
                          value={editingName}
                          onChange={(e) => handleEditingNameChange(e.target.value)}
                          onBlur={handleFinishEditing}
                          onKeyDown={(e) => {
                            if (e.key === 'Enter') {
                              handleFinishEditing();
                            } else if (e.key === 'Escape') {
                              setIsEditingFilename(false);
                              setEditingName('');
                            }
                          }}
                          className="bg-transparent text-xs font-medium outline-none min-w-0 flex-1"
                          style={{ color: 'var(--foreground)' }}
                          autoFocus
                          onFocus={(e) => e.target.select()}
                        />
                        <span className="text-xs font-medium opacity-60 flex-shrink-0" style={{ color: 'var(--foreground)' }}>
                          .{getFileExtension(file.language)}
                        </span>
                      </div>
                    ) : (
                      <span
                        className="text-xs font-medium truncate min-w-0 flex-1"
                        style={{ color: 'var(--foreground)' }}
                        onDoubleClick={() => file.id === activeFileId && handleStartEditing()}
                      >
                        <span>{getFilenameWithoutExtension(file.filename)}</span>
                        <span className="opacity-60">.{getFileExtension(file.language)}</span>
                        {file.isModified && <span className="ml-1 opacity-80">•</span>}
                      </span>
                    )}

                    {/* Close Button */}
                    {files.length > 1 && (
                      <button
                        onClick={(e) => {
                          e.stopPropagation();
                          closeFile(file.id);
                        }}
                        className={`flex-shrink-0 transition-opacity p-1 ${file.id === activeFileId
                            ? 'opacity-60 hover:opacity-100'
                            : 'opacity-0 group-hover:opacity-60 hover:opacity-100'
                          }`}
                        style={{ color: 'var(--foreground)' }}
                      >
                        <svg width="8" height="8" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                          <line x1="18" y1="6" x2="6" y2="18" />
                          <line x1="6" y1="6" x2="18" y2="18" />
                        </svg>
                      </button>
                    )}
                  </div>
                ))}
              </div>

              {/* Code Editor */}
              <div className="flex-1 min-h-0 rounded-b-md overflow-hidden shadow-md" style={{
                boxShadow: '0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)'
              }}>
                <CodeEditor
                  value={code}
                  onChange={handleCodeChange}
                  language={language}
                  suggestedCode={suggestedCode || undefined}
                  showDiff={showDiff}
                  onApplyDiff={handleApplyFix}
                  onRejectDiff={handleRejectFix}
                />
              </div>
            </div>
          </div>

          {/* Output Section */}
          <div className="flex-1 min-h-0 flex flex-col p-4 animate-fade-in" style={{ backgroundColor: 'var(--background)' }}>
            <div className="flex-1 min-h-0">
              <Output
                output={output}
                error={error}
                isLoading={isLoading}
                executionTime={executionStats.time}
                memoryUsed={executionStats.memory}
                onAIFix={language !== 'sanskrit' ? handleAIFix : undefined}
                isFixingCode={isFixingCode}
                stdin={stdin}
                onStdinChange={setStdin}
                code={code}
              />
            </div>
          </div>
        </div>

        {/* Desktop Layout - Resizable panels */}
        <div className="hidden lg:block h-full">
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
                      onClick={createNewFile}
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
                {/* Minimal Tabs attached to code editor */}
                <div className="flex-1 min-h-[300px] flex flex-col">
                  {/* Tabs */}
                  <div className="flex items-center overflow-x-auto scrollbar-hide">
                    {files.map((file) => (
                      <div
                        key={file.id}
                        className={`flex items-center gap-2 px-3 py-2 cursor-pointer group min-w-0 rounded-t-md transition-all duration-200 ${file.id === activeFileId
                          ? 'shadow-sm'
                          : 'hover:bg-gray-50 dark:hover:bg-gray-800'
                          }`}
                        style={{
                          backgroundColor: file.id === activeFileId && theme === 'dark' ? '#1e1e1e' : 'transparent',
                          boxShadow: file.id === activeFileId
                            ? '0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)'
                            : 'none'
                        }}
                        onClick={() => switchToFile(file.id)}
                      >
                        {/* File Icon */}
                        <div className="flex-shrink-0">
                          {isOmIcon(getLanguageIcon(file.language)) ? (
                            <span
                              style={{
                                fontSize: '14px',
                                fontWeight: 'bold',
                                color: file.id === activeFileId ? '#fbbf24' : 'currentColor'
                              }}
                            >
                              ॐ
                            </span>
                          ) : isLispIcon(getLanguageIcon(file.language)) ? (
                            <img
                              src="/lisp.png"
                              alt="Lisp"
                              style={{
                                width: '14px',
                                height: '14px',
                                filter: file.id === activeFileId ? 'sepia(1) saturate(3) hue-rotate(35deg)' : 'none'
                              }}
                            />
                          ) : isAssemblyIcon(getLanguageIcon(file.language)) ? (
                            <img
                              src={theme === 'dark' ? '/assemblydark.PNG' : '/assemblylight.PNG'}
                              alt="Assembly"
                              style={{
                                width: '16px',
                                height: '16px',
                                filter: file.id === activeFileId ? 'sepia(1) saturate(3) hue-rotate(35deg)' : 'none'
                              }}
                            />
                          ) : isBasicIcon(getLanguageIcon(file.language)) ? (
                            <img
                              src={theme === 'dark' ? '/basicdark.PNG' : '/basiclight.PNG'}
                              alt="Basic"
                              style={{
                                width: '12px',
                                height: '12px',
                                filter: file.id === activeFileId ? 'sepia(1) saturate(3) hue-rotate(35deg)' : 'none'
                              }}
                            />
                          ) : (
                            <i
                              className={getLanguageIcon(file.language)}
                              style={{
                                fontSize: '14px',
                                color: file.id === activeFileId ? '#fbbf24' : 'currentColor'
                              }}
                            ></i>
                          )}
                        </div>

                        {/* Filename */}
                        {isEditingFilename && file.id === activeFileId ? (
                          <div className="flex items-center min-w-0">
                            <input
                              type="text"
                              value={editingName}
                              onChange={(e) => handleEditingNameChange(e.target.value)}
                              onBlur={handleFinishEditing}
                              onKeyDown={(e) => {
                                if (e.key === 'Enter') {
                                  handleFinishEditing();
                                } else if (e.key === 'Escape') {
                                  setIsEditingFilename(false);
                                  setEditingName('');
                                }
                              }}
                              className="bg-transparent text-xs font-medium outline-none min-w-0 flex-1"
                              style={{ color: 'var(--foreground)' }}
                              autoFocus
                              onFocus={(e) => e.target.select()}
                            />
                            <span className="text-xs font-medium opacity-60 flex-shrink-0" style={{ color: 'var(--foreground)' }}>
                              .{getFileExtension(file.language)}
                            </span>
                          </div>
                        ) : (
                          <span
                            className="text-xs font-medium truncate min-w-0 flex-1"
                            style={{ color: 'var(--foreground)' }}
                            onDoubleClick={() => file.id === activeFileId && handleStartEditing()}
                          >
                            <span>{getFilenameWithoutExtension(file.filename)}</span>
                            <span className="opacity-60">.{getFileExtension(file.language)}</span>
                            {file.isModified && <span className="ml-1 opacity-80">•</span>}
                          </span>
                        )}

                        {/* Edit Button - Only show for active file on hover */}
                        {file.id === activeFileId && (
                          <button
                            onClick={(e) => {
                              e.stopPropagation();
                              handleStartEditing();
                            }}
                            className="flex-shrink-0 opacity-0 group-hover:opacity-60 hover:opacity-100 transition-opacity p-1"
                            style={{ color: 'var(--foreground)' }}
                            title="Edit filename"
                          >
                            <svg width="10" height="10" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                              <path d="m13.5 3.5-11 11V19h4.5l11-11a1.5 1.5 0 0 0 0-2.12l-2.38-2.38a1.5 1.5 0 0 0-2.12 0Z" />
                              <path d="m13.5 6.5 3 3" />
                            </svg>
                          </button>
                        )}

                        {/* Close Button */}
                        {files.length > 1 && (
                          <button
                            onClick={(e) => {
                              e.stopPropagation();
                              closeFile(file.id);
                            }}
                            className={`flex-shrink-0 transition-opacity p-1 ${file.id === activeFileId
                                ? 'opacity-60 hover:opacity-100'
                                : 'opacity-0 group-hover:opacity-60 hover:opacity-100'
                              }`}
                            style={{ color: 'var(--foreground)' }}
                          >
                            <svg width="10" height="10" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                              <line x1="18" y1="6" x2="6" y2="18" />
                              <line x1="6" y1="6" x2="18" y2="18" />
                            </svg>
                          </button>
                        )}
                      </div>
                    ))}
                  </div>

                  {/* Code Editor */}
                  <div className="flex-1 rounded-b-md overflow-hidden shadow-md" style={{
                    boxShadow: '0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)'
                  }}>
                    <CodeEditor
                      value={code}
                      onChange={handleCodeChange}
                      language={language}
                      suggestedCode={suggestedCode || undefined}
                      showDiff={showDiff}
                      onApplyDiff={handleApplyFix}
                      onRejectDiff={handleRejectFix}
                    />
                  </div>
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
                    onAIFix={language !== 'sanskrit' ? handleAIFix : undefined}
                    isFixingCode={isFixingCode}
                    stdin={stdin}
                    onStdinChange={setStdin}
                    code={code}
                  />
                </div>
              </div>
            }
          />
        </div>
      </div>

      {/* Info Overlay */}
      <InfoOverlay />
    </div>
  );
}
