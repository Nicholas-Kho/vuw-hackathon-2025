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

  useEffect(() => {
    fetch('/api/hello/')
      .then((res) => res.json())
      .then((data) => setMessage(data.message));
  }, []);
    useEffect(() => {
      fetch('/api/search/', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({})
      })
        .then((res) => res.json()) // <== parses response JSON
        .then((data) => {
          setResults(data);         // <== stores it in React state
        });
    }, []);

  return (
  <div class="container">
    <div className="container1">
      <nav className="sidebar">
        <h2>Menu</h2>
        <a href="#">Home</a>
        <a href="#">About</a>
        <a href="#">Contact</a>
      </nav>
      <div className="main-content">
        <h1>
        <BlurText
  text="Isn't this so cool?!"
  delay={150}
  animateBy="words"
  direction="top"
  onAnimationComplete={handleAnimationComplete}
  className="text-2xl mb-8"
/></h1>


        <p>This is the main content area.</p>
      </div>
    </div>

    <div class="silk_background">
      <Silk
      speed={5}
      scale={1}
      color="#7B7481"
      noiseIntensity={1.5}
      rotation={0}
    />
    </div>
    <div class="contents">
  <p>yo</p>
    <h1>{message}</h1>         {/* from /api/hello/ */}
<pre>{JSON.stringify(results, null, 2)}</pre>  {/* from /api/search/ */}
</div>
    </div>
  );
}

export default App;