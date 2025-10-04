'use client';

import { SparkIcon } from './SparkIcon';
import { useTheme } from './ThemeProvider';

interface OutputProps {
  output: string;
  error: string;
  isLoading: boolean;
  executionTime: number | null;
  memoryUsed: number | null;
  onAIFix?: () => void;
  isFixingCode?: boolean;
}

export default function Output({ 
  output, 
  error, 
  isLoading, 
  executionTime, 
  memoryUsed, 
  onAIFix,
  isFixingCode = false 
}: OutputProps) {
  const formatExecutionTime = (time: number | null) => {
    if (time === null) return null;
    if (time < 0.001) return '<1ms';
    if (time < 1) return `${Math.round(time * 1000)}ms`;
    return `${time.toFixed(3)}s`;
  };

  const formatMemory = (memory: number | null) => {
    if (memory === null) return null;
    if (memory < 1024) return `${memory}KB`;
    return `${(memory / 1024).toFixed(1)}MB`;
  };

  const showStats = !isLoading && (output || error) && (executionTime !== null || memoryUsed !== null);

  return (
    <div className="h-full flex flex-col" style={{ backgroundColor: 'var(--background)' }}>
      <div className="p-6 pb-4" style={{ backgroundColor: 'var(--background)' }}>
        <div className="flex items-center justify-between">
          <div className="text-xl font-light" style={{ color: 'var(--foreground)' }}>Output</div>
          {showStats && (
            <div className="flex items-center gap-4 text-xs font-mono opacity-60" style={{ color: 'var(--foreground)' }}>
              {executionTime !== null && (
                <span>{formatExecutionTime(executionTime)}</span>
              )}
              {memoryUsed !== null && (
                <span>{formatMemory(memoryUsed)}</span>
              )}
            </div>
          )}
        </div>
      </div>
      
      <div className="flex-1 flex flex-col p-6 font-mono text-sm leading-relaxed" style={{ backgroundColor: 'var(--background)' }}>
        {/* Output Content */}
        <div className="flex-1 overflow-auto">
          {isLoading ? (
            <div style={{ color: 'var(--foreground)' }} className="flex items-center gap-1">
              Running code
              <span className="inline-flex">
                <span className="animate-pulse" style={{ animationDelay: '0ms' }}>.</span>
                <span className="animate-pulse" style={{ animationDelay: '200ms' }}>.</span>
                <span className="animate-pulse" style={{ animationDelay: '400ms' }}>.</span>
              </span>
            </div>
          ) : (
            <>
              {error ? (
                <div className="animate-fade-in">
                  <div style={{ color: 'var(--foreground)' }} className="whitespace-pre-wrap mb-3">
                    {error}
                  </div>
                  {onAIFix && (
                    <span
                      onClick={onAIFix}
                      className="group inline-flex items-center gap-1.5 cursor-pointer hover:opacity-70 disabled:opacity-50 disabled:cursor-not-allowed transition-opacity duration-200 text-xs font-semibold tracking-wide"
                      style={{ 
                        color: 'var(--foreground)', 
                        fontFamily: 'SF Pro Display, -apple-system, BlinkMacSystemFont, system-ui, sans-serif' 
                      }}
                      title="Fix code with AI"
                    >
                      <div className="group-hover:rotate-12 transition-transform duration-200">
                        <SparkIcon size={12} />
                      </div>
                      {isFixingCode ? 'Fixing...' : 'AI Fix'}
                    </span>
                  )}
                </div>
              ) : output ? (
                <div style={{ color: 'var(--foreground)' }} className="whitespace-pre-wrap animate-fade-in">
                  {output}
                </div>
              ) : (
                <div style={{ color: 'var(--foreground)' }} className="opacity-50 animate-fade-in">
                  Click Run to execute your code
                </div>
              )}
            </>
          )}
        </div>
        

      </div>
    </div>
  );
}