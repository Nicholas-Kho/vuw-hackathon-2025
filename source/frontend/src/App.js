import { useEffect, useState } from 'react';

function App() {
  const [message, setMessage] = useState('');
  const [results, setResults] = useState([]);
  

  return (
  <div>
  <p>yo</p>
    <h1>{message}</h1>         {/* from /api/hello/ */}
<pre>{JSON.stringify(results, null, 2)}</pre>  {/* from /api/search/ */}

    </div>
  );
}

export default App;
