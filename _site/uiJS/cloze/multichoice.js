//multichoice.js

// Import modules
import { scrambleIntegerList, maxsize, mk_tooltip_divs } from './tools.js';

//TODO:
// fazer addcloze_multichoice (sem _scramble)
export function addcloze_multichoice_s(formname, divname, {
    options = ['uma opção correta', 'w.op1'], 
    feedback = ['op correct', 'op1 feedback'],
    points = [100, -10]} = {}) {    

    // Common starting part
    const formDiv = document.forms[formname]; //useless?
    //const targetDiv = document.querySelector(`div[name="${divname}"]`);
    const targetDiv = document.querySelector(divname);

    //Assert options, feedback and points has same length
    //TODO

    // Create dropdown
    const select = document.createElement('select');
    const defaultOption = document.createElement('option');
    defaultOption.value = '';
    defaultOption.text = 'Select an option';
    select.appendChild(defaultOption);



    // Scramble options
    const _integer_list_scrambled = scrambleIntegerList(options.length);//starts at 0

    const scrambled_options = _integer_list_scrambled.map(index => options[index]);
    const scrambled_feedback = _integer_list_scrambled.map(index => feedback[index]);
    const scrambled_points = _integer_list_scrambled.map(index => points[index]);



    // Add options to dropdown
    // options is get from parameters like options = ['Option 1', 'Option 2', 'Option 3']

    scrambled_options.forEach((optionText, index) => {
        const option = document.createElement('option');
        option.value = index; 
        option.text = optionText;
        select.appendChild(option);
    });

    // select.classList.add("clozeitem")

    const maxWidth = maxsize(options);
    select.style.width = (maxWidth + 40) + 'px';

    //small box at right where '✔' or '✖' appears;
    const validationBox = document.createElement('div');
    validationBox.style.display = 'inline-block';
    validationBox.className = 'validation-box-space';
    validationBox.textContent = '';
    //validationBox.id = id;


    //tool tip container
    const [tooltipContainer, tooltip] = mk_tooltip_divs();

    //Adding this containers to targetDiv (div of the multichoicebox)
    targetDiv.appendChild(select);
    targetDiv.appendChild(validationBox);
    targetDiv.appendChild(tooltipContainer);

    //  TODO inserir "tooltipContainer" já a seguir:

    select.addEventListener('change', function() {
        const selectedIndex = parseInt(select.value);
        const points_of_selected_option = scrambled_points[selectedIndex];
        let feedback_of_selected_option = scrambled_feedback[selectedIndex];

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
    });

    //select.classList = select.parentElement.classList;

}
