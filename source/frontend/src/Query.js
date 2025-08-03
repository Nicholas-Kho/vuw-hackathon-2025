import "./Query.css"

import { useEffect, useState } from 'react';

function getImageUrl(item) {
  return (
    item.hasRepresentation?.find(r => r.contentUrl)?.contentUrl ||
    item.image?.url ||
    null // or default path like a "no image found" image which is kinda done below...
  );
}

function Query() {
  const [searchQuery, setSearchQuery] = useState('Nightshade');
  const [previousWord, setPreviousWord] = useState('');
  const [placeholder, setPlaceholder] = useState('Search...');

  // User word clicking changes the word
  const handleWordClick = (word) => {
    setPreviousWord(searchQuery);     // Save current before overwriting
    setSearchQuery(word);             // Set new word
  };

  console.log("Begin query");
  const [results, setResults] = useState([]);

  // Button to search
  const fetchResults = () => {
    fetch('/api/search/', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ query: searchQuery })
    })
      .then((res) => {
        if (!res.ok) {
          throw new Error(`Server error: ${res.status}`);
        }
        return res.json();
      })
      .then((data) => {
        setResults(data.results ?? []);
      })
      .catch((err) => {
        console.error('Fetch error:', err);
      });
  };

  useEffect(() => {
    fetchResults();
  }, [searchQuery]); // Only runs when searchQuery changes



  // Send the query request
  useEffect(() => { // Honestly I don't care what this pulls anymore, I'm just going to read whatever comes through and wait until the backend fixes
    fetch('/api/search/', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ query: searchQuery })

    })
      .then((res) => {
        if (!res.ok) {
          throw new Error(`Server error: ${res.status}`);
        }
        return res.json();
      })
      .then((data) => {
        // console.log('Data received:', data);

        setResults(data.results ?? []); // Only use .results if it's an array
        // console.log('Filtered Results:', filtered);
        // setResults(data); // Save only relevant results
      })
      .catch((err) => {
        console.error('Fetch error:', err);
      });
  }, []);

  return (
    // Output here :)

    // Format so that an image will be along the left with the relevaant text along the right of it

    // Add relevant links along the bottom


    // Sort through every single given input (if this works it'll bless the world)
    <div>


      {/*<button onClick={fetchResults}>Resend Query</button>*/}
      <div class="query-header">
      <input
        class="display-bar"
        type="text"
        placeholder={searchQuery}
        value={searchQuery}
      />
      <button className="revert-button" onClick={() => setSearchQuery(previousWord)}>
        Revert to Previous Word
      </button>
      </div>

      {Array.isArray(results) && results.length === 0 && <p>No results found.</p>}
      {results.map((item, index) => {
        console.log("Item: " + item);
        const id = item.id ?? "None Found.";
        const imageUrl = getImageUrl(item);
        const title = item.title ?? "Untitled";
        const description = item.caption ?? "No description available";
        const href = item.href ?? "#";
        const creator =
          item.evidenceFor?.atEvent?.recordedBy?.[0]?.title ||
          item.identification?.find(i => i?.identifiedBy?.title)?.identifiedBy?.title ||
          "Unknown";

        const createdDate =
          item.evidenceFor?.atEvent?.eventDate ||
          item._meta?.created?.split("T")[0] ||
          "Date not specified";

        return (
          <div key={index} className="result-card">
            {imageUrl ? (
              <img src={imageUrl} alt={title} />
            ) : (
              <div className="result-card-placeholder">No image found :(</div>
            )}
            <div className="result-card-content">
              <h2>{title}</h2>
              <p><strong>ID:</strong> {id}</p>
              <p><strong>Creator:</strong> {creator}</p>
              <p><strong>Date:</strong> {createdDate}</p>
              <p>
                {description.split(' ').map((word, i) => (
                  <span
                    key={i}
                    onClick={() => handleWordClick(word)}
                    className="clickable-word"
                  >
                    {word}
                  </span>
                ))}
              </p>
            </div>
          </div>

        );
      })}
    </div>
  );
} // NightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmare
// <pre>{JSON.stringify(results, null, 2)}</pre>
export default Query;