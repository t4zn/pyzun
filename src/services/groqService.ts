interface FixCodeResponse {
  fixedCode?: string;
  error?: string;
}

export async function fixCodeWithAI(code: string, error: string, language: string): Promise<string> {
  try {
    const response = await fetch('/api/fix-code', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        code,
        error,
        language,
      }),
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.error || `API error: ${response.status}`);
    }

    const data: FixCodeResponse = await response.json();
    
    if (!data.fixedCode) {
      throw new Error('No code suggestion received from AI');
    }

    return data.fixedCode;
  } catch (error) {
    console.error('Error calling fix-code API:', error);
    throw error;
  }
}