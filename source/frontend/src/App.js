import { useEffect, useState } from 'react';

function App() {
  const [message, setMessage] = useState('');
  const [results, setResults] = useState([]);

  useEffect(() => {
    fetch('/api/hello/')
      .then((res) => res.json())
      .then((data) => setMessage(data.message));
  }, []);
    useEffect(() => {
      fetch('/api/search/', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ query: 'Manuhiri' })
      })
        .then((res) => res.json()) // <== parses response JSON
        .then((data) => {
          setResults(data);         // <== stores it in React state
        });
    }, []);

  return (
  <div>
  <p>yo</p>
    <h1>{message}</h1>         {/* from /api/hello/ */}
<pre>{JSON.stringify(results, null, 2)}</pre>  {/* from /api/search/ */}

    </div>
  );
}

export default App;
