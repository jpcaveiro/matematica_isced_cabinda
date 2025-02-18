//multichoice.js

// Import modules
import { mk_tooltip_divs, isStrictNumber } from './tools.js';

/*
addcloze_numerical('q2_input_number', {
    options: [120.1, 130.3, 0], 
    points: [10, 5, -30],
    tolerance: [0.1, 1, 10000],
    feedback: ['op correct', 'op1 feedback', 'op2 feedback']
})
*/

export function addcloze_numerical(formname, divname, {
    options = [120.1, 130.3], 
    tolerance = [0.1, 1],
    feedback = ['op correct', 'feedback partial correct 1', 'wrong number'], //1 more feedback
    points = [10, 5, -30] //1 more points
    } = {}) {    

    // Common starting part
    const formDiv = document.forms[formname]; //useless?
    //const targetDiv = document.querySelector(`div[name="${divname}"]`);
    const targetDiv = document.querySelector(divname);

    //Assert options, feedback and points has same length
    //TODO

    const inputElement = document.createElement('input');
    inputElement.type = 'text';
    const widthCm = 3; //3 cm
    const pixels = Math.round(widthCm * 96 / 2.54);  // Round to nearest pixel
    inputElement.style.width = `${pixels}px`;
    //inputElement.id = inputId; //qual será o id se não for dado?

    //small box at right where '✔' or '✖' appears;
    const validationBox = document.createElement('div');
    validationBox.style.display = 'inline-block';
    validationBox.className = 'validation-box-space';
    validationBox.textContent = '';
    //validationBox.id = id;


    //tool tip container
    const [tooltipContainer, tooltip] = mk_tooltip_divs();

    //Adding this containers to targetDiv (div of the multichoicebox)
    targetDiv.appendChild(inputElement);
    targetDiv.appendChild(validationBox);
    targetDiv.appendChild(tooltipContainer);

    const numbers = [10, 20, 30, 40, 50];
    const valueToSubtract = 5;
    
    const newNumbers = numbers.map(number => number - valueToSubtract);
   
    console.log(newNumbers); // Output: [5, 15, 25, 35, 45]
    console.log(numbers);    // Output: [10, 20, 30, 40, 50] (Original array unchanged)

    //to be checked: "input" on the next instruction means an event (not a name)
    /*'input' - it means the user has modified the value of the 
    input field in some way.  
    
    This can include:

    Typing: Adding or deleting characters.
    Pasting: Pasting text into the field.
    Cutting: Cutting text from the field.
    Dragging and dropping: Dragging and dropping text into the field.
    Using auto-complete: Selecting a suggestion from the browser's autocomplete.
    Voice input: Using voice-to-text to enter data.
    Changing input type specific actions: For example, on a number input, even clicking the up/down arrows triggers the input event.
    */
    /*inputElement.addEventListener('input', function(event) { 

        let value = event.target.value;
        console.log(value)
        if (!/^\d*\.?\d*$/.test(value)) {
            event.target.value = value.slice(0, -1);
        }
    });
    */

    function update_numerical(inputValue) {


        //options here is a list of values (see arguments)
        const differences = options.map(number => Math.abs(number - inputValue));

        /* Estudar esta solução
        function indexOfSmallestReduce(arr) {
            if (arr.length === 0) return -1;

            return arr.reduce((minIndex, currentValue, currentIndex) => {
            return currentValue < arr[minIndex] ? currentIndex : minIndex;
            }, 0); // Start with index 0
        }
        */

        let smallestDifValue = differences[0];
        let indexOfSmallestDifference = 0;

        for (let i = 1; i < differences.length; i++) {
            if (differences[i] < smallestDifValue) {
            smallestDifValue = differences[i];
            indexOfSmallestDifference = i;
            }
        }

        if ( smallestDifValue > tolerance[indexOfSmallestDifference] ) {
            indexOfSmallestDifference = points.length-1; //last points is for wrong answer
        }

        const points_of_selected_option = points[indexOfSmallestDifference];
        let feedback_of_selected_option = feedback[indexOfSmallestDifference];

        if (feedback_of_selected_option === undefined || 
            feedback_of_selected_option.length === 0) {
            feedback_of_selected_option = 'Not available.';
        }

        tooltip.textContent = feedback_of_selected_option;

        //console.log(selectedValue);

        if (points_of_selected_option === 100) { 
            validationBox.className = 'validation-box correct';
            validationBox.textContent = '✔';
            tooltipContainer.style.visibility = 'hidden';
            tooltip.style.visibility =  'hidden';
        } else if (points_of_selected_option > 0) {
            validationBox.className = 'validation-box partiallycorrect';
            validationBox.textContent = '!';
            tooltipContainer.style.visibility = 'visible';
            tooltip.style.visibility =  'visible';
        } else if (points_of_selected_option <= 0) {
            validationBox.className = 'validation-box incorrect';
            validationBox.textContent = '✖';
            tooltipContainer.style.visibility = 'visible';
            tooltip.style.visibility =  'visible';
        } else {
            validationBox.className = 'validation-box-space';
            validationBox.textContent = '';
            tooltipContainer.style.visibility = 'hidden';
            tooltip.style.visibility =  'hidden';
        }
    };

    inputElement.addEventListener('blur', function(event) {
        event.preventDefault();

        //console.log(event);

        const inputText = inputElement.value;

        if (isStrictNumber(inputText)) {
            const inputValue = Number(inputText);
            update_numerical(inputValue);
        }
        

    });

    inputElement.addEventListener('keydown', function(event) {

        //event.preventDefault();

        //console.log(event.key);

        const inputValue = Number(inputElement.value);

        if (event.key === 'enter') {
            // Prevent the default form submission behavior (if inside a form)
            event.preventDefault();
            
            const inputText = inputElement.value;

            if (isStrictNumber(inputText)) {
                const inputValue = Number(inputText);
                update_numerical(inputValue);
            }

        } else {

            if (! /^\d*\.?\d*$/.test(inputElement.value) ) {
                event.target.value = ""; //inputElement.value.slice(0, -1);
            }

        }
    });

    //select.classList = select.parentElement.classList;

}
