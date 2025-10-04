'use client';

import { useState, useRef, useEffect, ReactNode } from 'react';

interface ResizablePanelsProps {
  leftPanel: ReactNode;
  rightPanel: ReactNode;
  defaultLeftWidth?: number;
  minLeftWidth?: number;
  maxLeftWidth?: number;
}

export default function ResizablePanels({
  leftPanel,
  rightPanel,
  defaultLeftWidth = 50,
  minLeftWidth = 20,
  maxLeftWidth = 80
}: ResizablePanelsProps) {
  const [leftWidth, setLeftWidth] = useState(defaultLeftWidth);
  const [isDragging, setIsDragging] = useState(false);
  const containerRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const handleMouseMove = (e: MouseEvent) => {
      if (!isDragging || !containerRef.current) return;

      const containerRect = containerRef.current.getBoundingClientRect();
      const newLeftWidth = ((e.clientX - containerRect.left) / containerRect.width) * 100;
      
      // Clamp the width between min and max values
      const clampedWidth = Math.min(Math.max(newLeftWidth, minLeftWidth), maxLeftWidth);
      setLeftWidth(clampedWidth);
    };

    const handleMouseUp = () => {
      setIsDragging(false);
    };

    if (isDragging) {
      document.addEventListener('mousemove', handleMouseMove);
      document.addEventListener('mouseup', handleMouseUp);
      document.body.style.cursor = 'col-resize';
      document.body.style.userSelect = 'none';
    }

    return () => {
      document.removeEventListener('mousemove', handleMouseMove);
      document.removeEventListener('mouseup', handleMouseUp);
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
    };
  }, [isDragging, minLeftWidth, maxLeftWidth]);

  const handleMouseDown = () => {
    setIsDragging(true);
  };

  return (
    <div ref={containerRef} className="flex h-full">
      {/* Left Panel */}
      <div 
        style={{ width: `${leftWidth}%` }}
        className="flex-shrink-0"
      >
        {leftPanel}
      </div>

      {/* Resizer */}
      <div
        className="w-6 bg-transparent cursor-col-resize flex-shrink-0 group flex items-center justify-center"
        onMouseDown={handleMouseDown}
      >
        <div className="flex flex-col gap-1 opacity-30 group-hover:opacity-60 transition-opacity duration-200">
          <div 
            className="w-1 h-1 rounded-full"
            style={{ backgroundColor: 'var(--foreground)' }}
          />
          <div 
            className="w-1 h-1 rounded-full"
            style={{ backgroundColor: 'var(--foreground)' }}
          />
          <div 
            className="w-1 h-1 rounded-full"
            style={{ backgroundColor: 'var(--foreground)' }}
          />
          <div 
            className="w-1 h-1 rounded-full"
            style={{ backgroundColor: 'var(--foreground)' }}
          />
          <div 
            className="w-1 h-1 rounded-full"
            style={{ backgroundColor: 'var(--foreground)' }}
          />
        </div>
      </div>

      {/* Right Panel */}
      <div 
        style={{ width: `${100 - leftWidth}%` }}
        className="flex-shrink-0"
      >
        {rightPanel}
      </div>
    </div>
  );
}