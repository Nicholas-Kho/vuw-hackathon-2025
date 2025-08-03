import React, { useState, useEffect } from 'react';
import InfiniteScroll from '../React Components/InfiniteScroll';

const ScrollTheme = ({ theme }) => {
  const [results, setResults] = useState([]);
  const [page, setPage] = useState(1);
  const [hasMore, setHasMore] = useState(true);
  const [isLoading, setIsLoading] = useState(false);
  const pageSize = 10;

  const fetchResults = async () => {
    setIsLoading(true);
    try {
      const res = await fetch('/api/search/', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ query: theme, page, limit: pageSize }),
      });
      const data = await res.json();

      setResults((prev) => [...prev, ...data]);
      setHasMore(data.length === pageSize);
      setPage((prev) => prev + 1);
    } catch (err) {
      console.error('Failed to load results', err);
    } finally {
      setIsLoading(false);
    }
  };

  // Reset when theme changes
  useEffect(() => {
    setResults([]);
    setPage(1);
    setHasMore(true);
  }, [theme]);

  // Fetch initial data on mount or when theme changes
  useEffect(() => {
    fetchResults();
  }, [theme]);

  return (
    <InfiniteScroll loadMore={fetchResults} hasMore={hasMore} isLoading={isLoading}>
      <ul style={{ maxWidth: 600, margin: '0 auto', padding: 0 }}>
        {results.map((item, index) => (
          <li key={index} style={{ borderBottom: '1px solid #ddd', padding: '10px 0' }}>
            <h4>{item.title || `Item ${index + 1}`}</h4>
            <p>{item.body || JSON.stringify(item)}</p>
          </li>
        ))}
      </ul>
    </InfiniteScroll>
  );
};

export default ScrollTheme;
