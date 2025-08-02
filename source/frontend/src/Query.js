import { useEffect, useState } from 'react';

function getImageUrl(item) {
  return (
    item.hasRepresentation?.find(r => r.contentUrl)?.contentUrl ||
    item.image?.url ||
    null // or default path like a "no image found" image which is kinda done below...
  );
}

function Query() {
  console.log("Begin query");
  const [results, setResults] = useState([]);

  useEffect(() => { // Honestly I don't care what this pulls anymore, I'm just going to read whatever comes through and wait until the backend fixes
    fetch('/api/search/', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ query: 'Nightshade' })
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
      <h1>Query made :)</h1>
      
      {Array.isArray(results) && results.length === 0 && <p>No results found.</p>}
      {results.map((item, index) => {
        console.log("Item: "+item);
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
          <div key={index} style={{ display: 'flex', marginBottom: '2rem', alignItems: 'flex-start' }}>
            {imageUrl ? (
              <img
                src={imageUrl}
                alt={title}
                style={{
                  width: '200px',
                  height: 'auto',
                  marginRight: '1rem',
                  objectFit: 'cover',
                }}
              />
            ) : (
              <div
                style={{
                  width: '200px',
                  height: '200px',
                  marginRight: '1rem',
                  backgroundColor: '#eee',
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center',
                  color: '#666',
                  fontStyle: 'italic',
                  fontSize: '0.9rem',
                  border: '1px solid #ccc',
                }}
              >
                No image found :(
              </div>
            )}
            <div>
              <h2>{title}</h2>
              <p>ID: {id}</p>
              <p><strong>Creator:</strong> {creator}</p>
              <p><strong>Date:</strong> {createdDate}</p>
              <p>{description}</p>


            </div>
          </div>
        );
      })}
      <pre>{JSON.stringify(results, null, 2)}</pre>
    </div>
  );
} // NightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmareNightmare
// <pre>{JSON.stringify(results, null, 2)}</pre>
export default Query;