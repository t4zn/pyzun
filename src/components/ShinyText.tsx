'use client';

import React from 'react';

interface ShinyTextProps {
  text: string;
  disabled?: boolean;
  speed?: number;
  className?: string;
}

const ShinyText: React.FC<ShinyTextProps> = ({
  text,
  disabled = false,
  speed = 3,
  className = ''
}) => {
  if (disabled) {
    return (
      <span className={`opacity-50 ${className}`} style={{ color: 'var(--foreground)' }}>
        {text}
      </span>
    );
  }

  return (
    <span
      className={`animate-shimmer ${className}`}
      style={{
        background: `linear-gradient(90deg, 
          var(--foreground) 0%, 
          var(--foreground) 40%, 
          rgba(255, 255, 255, 0.9) 45%, 
          rgba(255, 255, 255, 1) 50%, 
          rgba(255, 255, 255, 0.9) 55%, 
          var(--foreground) 60%, 
          var(--foreground) 100%)`,
        backgroundSize: '200% 100%',
        WebkitBackgroundClip: 'text',
        WebkitTextFillColor: 'transparent',
        backgroundClip: 'text',
        animationDuration: `${speed}s`,
        animationTimingFunction: 'ease-in-out',
        animationIterationCount: 'infinite',
        display: 'inline-block',
        fontWeight: 'inherit'
      }}
    >
      {text}
    </span>
  );
};

export default ShinyText;