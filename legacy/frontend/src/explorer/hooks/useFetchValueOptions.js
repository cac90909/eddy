import { useState, useEffect } from 'react';
import handleOperation from '../services/ExplorerService';

// This hook accepts an operation name and a parameters object,
// and returns { options, loading, error }.
export function useValueOptions(operationName, operationArguments) {
  const [valueOptions, setValueOptions] = useState([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  useEffect(() => {
    if (!operationName) return;
    
    // Stringify parameters for dependency tracking.
    const argumentsKey = JSON.stringify(operationArguments);
    setLoading(true);
    setError(null);

    // Use the existing handleOperation function from ExplorerService.
    handleOperation(operationName, parameters)
      .then(response => {
        // Assume response contains either a field "options" or is the options array.
        const fetchedValueOptions = response.data || response;
        setValueOptions(fetchedValueOptions);
        setLoading(false);
      })
      .catch(err => {
        setError(err);
        setLoading(false);
      });
  }, [operationName, JSON.stringify(operationArguments)]);

  return { valueOptions, loading, error };
}

//incoorpoate useFetchValueOptions
