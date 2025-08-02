import { useEffect, useState } from 'react';

import Silk from './React Components/Silk';
import "./App.css"
import Iridescence from './React Components/Iridescence';
import BlurText from "./React Components/BlurText";
import Query from "./Query.js"

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
          {/* :mag: Search Bar */}
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
          <Query />
        </div>
        </div>
      </div>
    </div>
  );
}


export default App;