'use client';

import { useState } from 'react';
import { languages } from '../components/LanguageSelector';

interface Judge0Response {
  stdout?: string;
  stderr?: string;
  compile_output?: string;
  status: {
    id: number;
    description: string;
  };
  time?: string;
  memory?: number;
}

export function useJudge0() {
  const [isLoading, setIsLoading] = useState(false);

  const executeCode = async (code: string, language: string, stdin?: string) => {
    setIsLoading(true);
    
    try {
      const apiKey = process.env.NEXT_PUBLIC_RAPIDAPI_KEY;
      
      // Check if using demo key
      if (!apiKey || apiKey === 'demo-key') {
        // Simulate execution for demo purposes
        await new Promise(resolve => setTimeout(resolve, 2000));
        return {
          output: 'Demo mode: Please add your RapidAPI key to .env.local to execute code.\nGet your free key at: https://rapidapi.com/judge0-official/api/judge0-ce/',
          error: '',
          executionTime: null,
          memoryUsed: null
        };
      }
      
      const languageId = languages.find(lang => lang.value === language)?.id || 71;
      
      // Submit code for execution
      const submitResponse = await fetch('https://judge0-ce.p.rapidapi.com/submissions?base64_encoded=true&wait=false', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'X-RapidAPI-Key': apiKey,
          'X-RapidAPI-Host': 'judge0-ce.p.rapidapi.com'
        },
        body: JSON.stringify({
          language_id: languageId,
          source_code: btoa(code),
          stdin: stdin ? btoa(stdin) : undefined,
        })
      });

      if (!submitResponse.ok) {
        const errorText = await submitResponse.text();
        throw new Error(`Failed to submit code: ${submitResponse.status} ${errorText}`);
      }

      const submitData = await submitResponse.json();
      const token = submitData.token;

      // Poll for results
      let attempts = 0;
      const maxAttempts = 10;
      
      while (attempts < maxAttempts) {
        await new Promise(resolve => setTimeout(resolve, 1000));
        
        const resultResponse = await fetch(`https://judge0-ce.p.rapidapi.com/submissions/${token}?base64_encoded=true`, {
          headers: {
            'X-RapidAPI-Key': apiKey,
            'X-RapidAPI-Host': 'judge0-ce.p.rapidapi.com'
          }
        });

        if (!resultResponse.ok) {
          throw new Error(`Failed to get results: ${resultResponse.status}`);
        }

        const result: Judge0Response = await resultResponse.json();
        
        if (result.status.id > 2) { // Status > 2 means execution completed
          const output = result.stdout ? atob(result.stdout) : '';
          const error = result.stderr ? atob(result.stderr) : 
                       result.compile_output ? atob(result.compile_output) : '';
          
          return { 
            output, 
            error,
            executionTime: result.time ? parseFloat(result.time) : null,
            memoryUsed: result.memory || null
          };
        }
        
        attempts++;
      }
      
      throw new Error('Execution timeout - code took too long to execute');
    } catch (error) {
      console.error('Judge0 API error:', error);
      return { 
        output: '', 
        error: error instanceof Error ? error.message : 'An error occurred while executing the code',
        executionTime: null,
        memoryUsed: null
      };
    } finally {
      setIsLoading(false);
    }
  };

  return { executeCode, isLoading };
}