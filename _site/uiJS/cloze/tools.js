
/*
Scramble list of options

1. Get a random permutation from 0 ... n-1 ( = randperm)
2. Get options scrambled
3. Use randperm to get points and feedback

References:

https://gemini.google.com/app/62b2cdf73ed40a6d
*/


export function scrambleIntegerList(n) {

  // Method 1: Using Array.from() (Most concise and preferred)
  if (typeof n !== 'number' || n < 0 || !Number.isInteger(n)) {
    throw new Error("n must be a non-negative integer.");
  }
  const newList = Array.from({ length: n }, (_, i) => i);

  // Fisher-Yates shuffle algorithm for efficient and unbiased shuffling.
  for (let i = newList.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [newList[i], newList[j]] = [newList[j], newList[i]]; // Destructuring swap
  }

  return newList;
}




/*
  encode and decode
*/

// Encode JSON to base64
export function encode(jsonObj) {
  // 1. Convert JSON to string
  const jsonString = JSON.stringify(jsonObj);
  
  // 2. Convert string to base64
  const base64 = btoa(jsonString);
  
  return base64;
}

// Decode base64 back to JSON
export function decode(base64String) {
  try {
      // 1. Convert base64 to string
      const jsonString = atob(base64String);
      
      // 2. Parse string back to JSON
      const jsonObj = JSON.parse(jsonString);
      
      return jsonObj;
  } catch (error) {
      console.error('Error decoding:', error);
      return null;
  }
}




/*
  maximum size
*/

export function maxsize(options) {

  // Create invisible span to measure text width
  const span = document.createElement('span');
  span.style.position = 'absolute';
  span.style.visibility = 'hidden';
  span.style.whiteSpace = 'nowrap';
  document.body.appendChild(span);

  // Find the longest option text width
  let maxWidth = 0;
  options.forEach(text => {
      span.textContent = text;
      const width = span.offsetWidth;
      maxWidth = Math.max(maxWidth, width);
  });

  // Remove the measuring span
  document.body.removeChild(span);

  return maxWidth;
}



/* 
  tooltips for cloze answers
*/

export function mk_tooltip_divs() {

  // Create the container div
  const tooltipContainer = document.createElement('div');
  tooltipContainer.className = 'cloze-tooltip-container';

  // Create the question mark div
  const questionMark = document.createElement('div');
  questionMark.className = 'cloze-question-mark';
  questionMark.textContent = '?';

  // Create the tooltip div
  const tooltip = document.createElement('div');
  tooltip.className = 'cloze-tooltip';
  tooltip.textContent = 'This is some helpful text!';

  // Append the question mark and tooltip to the container
  tooltipContainer.appendChild(questionMark);
  tooltipContainer.appendChild(tooltip);

  // Append the container to the document body
  //document.body.appendChild(tooltipContainer);
  // This append is done after this function

  tooltipContainer.addEventListener('mousemove', (e) => {
      tooltip.style.opacity = '1';
      tooltip.style.left = (e.clientX + 10) + 'px';
      tooltip.style.top = (e.clientY + 10) + 'px';
  });

  tooltipContainer.addEventListener('mouseleave', () => {
      tooltip.style.opacity = '0';
  });

  return [tooltipContainer, tooltip];
}

/*
  check if a string is anumber
*/

export function isStrictNumber(str) {
  if (typeof str !== 'string') {
    return false; // Not a string
  }

  if (str.trim() === "") {
    return false; // Empty or whitespace only
  }

  // Use the unary plus operator to attempt conversion.  NaN will be returned if it fails.
  const num = +str;

  return typeof num === 'number' && !isNaN(num);
}
