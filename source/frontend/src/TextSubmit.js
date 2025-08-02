import { useState } from 'react';

function TextSubmit() {
  const [text, setText] = useState('');
  const [response, setResponse] = useState(null);

  const handleSubmit = (e) => {
    e.preventDefault();
    fetch('/api/submit-text/', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ text }),
    })
      .then((res) => res.json())
      .then((data) => setResponse(data.message))
      .catch((err) => console.error(err));
  };

  return (
    <div>
      <form onSubmit={handleSubmit}>
        <input
          type="text"
          placeholder="Enter text here"
          value={text}
          onChange={(e) => setText(e.target.value)}
          required
        />
        <button type="submit">Submit</button>
      </form>
      {response && <p>Server says: {response}</p>}
    </div>
  );
}

export default TextSubmit;
