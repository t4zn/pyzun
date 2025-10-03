'use client';

interface OutputProps {
  output: string;
  error: string;
  isLoading: boolean;
}

export default function Output({ output, error, isLoading }: OutputProps) {


  return (
    <div className="h-full flex flex-col" style={{ backgroundColor: 'var(--background)' }}>
      <div className="p-6 pb-4" style={{ backgroundColor: 'var(--background)' }}>
        <div className="text-xl font-light" style={{ color: 'var(--foreground)' }}>Output</div>
      </div>
      
      <div className="flex-1 flex flex-col p-6 font-mono text-sm leading-relaxed" style={{ backgroundColor: 'var(--background)' }}>
        {/* Output Content */}
        <div className="flex-1 overflow-auto">
          {isLoading ? (
            <div style={{ color: 'var(--foreground)' }} className="animate-pulse">
              Running code...
            </div>
          ) : (
            <>
              {error ? (
                <div style={{ color: 'var(--foreground)' }} className="whitespace-pre-wrap animate-fade-in">
                  {error}
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