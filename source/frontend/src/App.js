import { useEffect, useState } from 'react';

import Silk from './React Components/Silk';
import "./App.css"
import Iridescence from './React Components/Iridescence';
import BlurText from "./React Components/BlurText";

const handleAnimationComplete = () => {
  console.log('Animation completed!');
};






function App() {
  const [message, setMessage] = useState('');
  const [results, setResults] = useState([]);
  const [searchQuery, setSearchQuery] = useState('');

  const handleLinkClick = (word) => {
    setSearchQuery(word);
  };
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

    <div className="container">
      <p>yo</p>
    <h1>{message}</h1>         {/* from /api/hello/ */}
<pre>{JSON.stringify(results, null, 2)}</pre>  {/* from /api/search/ */}
      <div className="container1">
        <div className="sidebar">
          <div className="container2">
            <h2>Your Museum Item</h2>
            <img
              src="https://dummyimage.com/300x300/cccccc/000000&text=Hello"
              alt="Linked image"
              width="200"
              style={{ borderRadius: '10px' }}
            />
          </div>
          <h2>Links Clicked</h2>
          <ul>
            {['Maui', 'Tangaroa', 'Kiwi', 'Waka', 'Haka'].map((word, index) => (
              <li key={index} onClick={() => handleLinkClick(word)} style={{ cursor: 'pointer' }}>
                {word}
              </li>
            ))}
          </ul>
        </div>

        <div className="main-content">
          {/* 🔍 Search Bar */}
          <div className="search-bar">
            <input
              type="text"
              placeholder="Search..."
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
            />
          </div>

          {/* Card Content */}
          <div className="card-container">
  <div className="card-header">
    {/* Replace with actual image if available */}
    <img
      src="https://dummyimage.com/300x300/cccccc/000000&text=Powhiri"
      alt="[Powhiri]"
      className="card-image"
    />
    <h1 className="card-title">[Powhiri]</h1>
  </div>
  <p className="card-text">
    Depicts a large group of Māori people singing in front of a whare whakairo (carved house), while a man in a piupiu kneels before them holding a taiaha (fighting staff), as part of a formal challenge to manuhiri (visitors).
  </p>
  <p className="card-text" style={{ fontStyle: 'italic', fontSize: '0.9em' }}>
    Photographer: Brian Brake, 1980-1985, New Zealand<br />
    Collection: Photography — Te Papa (B.076443)
  </p>
  <a
    href="https://collections.tepapa.govt.nz/object/1239056"
    target="_blank"
    rel="noopener noreferrer"
    className="card-link"
  >
    View item at Te Papa →
  </a>
</div>
        </div>
      </div>
    </div>
  );
}


export default App;
