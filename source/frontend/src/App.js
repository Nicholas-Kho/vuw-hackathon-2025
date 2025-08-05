import { useEffect, useState } from 'react';

import Silk from './React Components/Silk';
import "./App.css"
import Iridescence from './React Components/Iridescence';
import BlurText from "./React Components/BlurText";
import Query from "./Query.js"
import TextSubmit from "./TextSubmit";

const handleAnimationComplete = () => {
  console.log('Animation completed!');
};






function App() {

  // Counter
  const [counter, setCounter] = useState(0);
  const [endingWord, setEndingWord] = useState('Benevolent Institution, Dunedin');

  // Clicked words
  const [clickedWords, setClickedWords] = useState([]);
  const incrementCounter = () => {
    setCounter(prev => prev + 1);
  };

  function handleWordClick(word) {
    console.log("Word clicked:", word);

  }

  const [message, setMessage] = useState('');
  const [won, setWin] = useState(false);
  const [results, setResults] = useState([]);
  const [searchQuery, setSearchQuery] = useState('');

    // Text handling
    const [text, setText] = useState('');
     const [isReadOnly, setIsReadOnly] = useState(false);

  const handleKeyDown = (e) => {
    if (e.key === 'Enter') {
      setIsReadOnly(true);  // Make input read-only on Enter
      e.preventDefault();   // Prevent form submission or newline if inside a form/textarea
    }
  };

  {/*sendText*/}
// React component or utility function to send data
const sendTextAndCounter = (text, counter, searchQuery, won) => {
  fetch('/api/submit-text/', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ text, counter, searchQuery, won }),
  })
  .then(res => res.json())
  .then(data => console.log('Server response:', data))
  .catch(err => console.error('Error:', err));
};





   // Handle what happens when a word is clicked
    const handleLinkClick = (word) => {
      setSearchQuery(word);

      setCounter((prevCounter) => {
        const newCounter = prevCounter + 1;

        // Use latest text, newCounter, word, and won from state and parameters
        sendTextAndCounter(text, newCounter, word, won);

        return newCounter;
      });

      setClickedWords(prev => [...prev, word]);
    };




  return (


    <div className="container">
              <nav className="navbar">

    <h1 className="navbar-title">MuseLinks -  Te Papa</h1>
  </nav>
      {/*<TextSubmit/>*/}
{/*<pre>{JSON.stringify(results, null, 2)}</pre>   from /api/search/ */}
      <div className="container1">
        <div className="sidebar">
          <div className="container2">
          {/* Name Text Box Enter*/}
            <div>
              <input
                type="text"
                value={text}
                readOnly={isReadOnly}
                onChange={(e) => setText(e.target.value)}
                onKeyDown={handleKeyDown}
                placeholder="What is your name?"
                style={{
                  padding: '10px',
                  fontSize: '16px',
                  borderRadius: '6px',
                  border: '1px solid #ccc',
                  width: '200px',
                  boxSizing: 'border-box',
                  outline: 'none',
                  transition: 'border-color 1s',
                  backgroundColor: isReadOnly ? '#eee' : 'white',
                  cursor: isReadOnly ? 'not-allowed' : 'text',
                }}
              />
              {/*<p>You typed: {text}</p>*/}
            </div>



            <h3>The museum Item: Hīnaki</h3>
            <img
              src="https://media.tepapa.govt.nz/collection/728/preview"
              alt="Linked image"
              width="200"
              style={{ borderRadius: '10px' }}
            />
            <h4>Description: Hīnaki (eel trap)</h4>
          </div>
          <h3>Score: {counter} Total Navigations</h3>
            <ul>
  {clickedWords.map((word, index) => (
    <li key={index}>{word}</li>
  ))}
</ul>
        </div>

        <div className="main-content">
          {/* :mag: Search Bar */}

          {/* Card Content (Query) */}
          <div className="query-container">
            <Query onWordClick={handleLinkClick}/>
          </div>

        </div>
      </div>
    </div>
  );
}


export default App;