'use client';

import { useState, useRef } from 'react';
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
  const currentKeyIndex = useRef(0);
  
  const getApiKeys = () => [
    process.env.NEXT_PUBLIC_RAPIDAPI_KEY,
    process.env.NEXT_PUBLIC_RAPIDAPI_KEY_1,
    process.env.NEXT_PUBLIC_RAPIDAPI_KEY_2
  ].filter(Boolean);
  
  const getCurrentApiKey = () => {
    const keys = getApiKeys();
    return keys[currentKeyIndex.current] || keys[0];
  };
  
  const switchToNextKey = () => {
    const keys = getApiKeys();
    currentKeyIndex.current = (currentKeyIndex.current + 1) % keys.length;
    console.log(`Switched to API key ${currentKeyIndex.current + 1}`);
  };
  
  const isRateLimitError = (response: Response, errorText?: string) => {
    return response.status === 429 || 
           response.status === 403 ||
           (errorText && errorText.toLowerCase().includes('rate limit'));
  };

  const executeCode = async (code: string, language: string, stdin?: string) => {
    setIsLoading(true);
    
    try {
      const keys = getApiKeys();
      
      // Check if any keys are available
      if (keys.length === 0 || keys.every(key => !key || key === 'demo-key')) {
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
      
      // Try each API key until one works or all fail
      let lastError: Error | null = null;
      const maxKeyAttempts = keys.length;
      
      for (let keyAttempt = 0; keyAttempt < maxKeyAttempts; keyAttempt++) {
        const apiKey = getCurrentApiKey();
        
        try {
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
            
            if (isRateLimitError(submitResponse, errorText)) {
              console.log(`Rate limit hit on key ${currentKeyIndex.current + 1}, switching to next key`);
              switchToNextKey();
              lastError = new Error(`Rate limit exceeded on API key ${keyAttempt + 1}`);
              continue; // Try next key
            }
            
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
              const errorText = await resultResponse.text();
              
              if (isRateLimitError(resultResponse, errorText)) {
                console.log(`Rate limit hit while polling results on key ${currentKeyIndex.current + 1}, switching to next key`);
                switchToNextKey();
                throw new Error('Rate limit exceeded while polling results');
              }
              
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
          if (error instanceof Error && error.message.includes('Rate limit exceeded')) {
            lastError = error;
            continue; // Try next key
          }
          throw error; // Re-throw non-rate-limit errors
        }
      }
      
      // If we get here, all keys failed due to rate limits
      throw lastError || new Error('All API keys have exceeded their rate limits');
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