// Essay
export function toggle_div(divname) { 
    const myDiv = document.getElementById(divname); /* Use ID */
    const myDivDisplay = myDiv.style.display; /* Use ID */
    if (myDivDisplay === 'none') {
      console.log('The div is hidden'); /* Displayed if hidden */
      myDiv.style.display = 'block'; /* Show the div */
    } else {
      console.log('The div is visible'); /* Displayed if visible */
      myDiv.style.display = 'none'; /* Hide the div */
    }
  }
  