// Todo este ficheiro é feito do lado do R (ou Python)
// Codificar as opções para não ficarem expostas ao user.

// https://docs.moodle.org/405/en/Embedded_Answers_(Cloze)_question_type#Detailed_syntax_explanations

import {addcloze_multichoice_s} from './cloze/multichoice.js';
import {addcloze_numerical} from './cloze/numerical.js';
import {addcloze_shortanswer} from './cloze/shortanswer.js';


export {addcloze_multichoice_s};
export {addcloze_numerical};
export {addcloze_shortanswer};


/*

Examples 

addcloze_multichoice_s('clozeform1', '#q1_dropdown', {
    options: ['uma opção correta', 'w.op1', 'w.op2', 'wop3'], 
    feedback: ['op correct', 'op1 feedback \\(\\sqrt{x}\\)', 'op2 feedback $\\sqrt{x}$', 'op3 feedback'],
    points: [100,30,0,0]
})


addcloze_multichoice_s('clozeform1', '#q3_dropdown', {
    options: ['3uma opção correta', '3w.op1', '3w.op2', '3wop3'], 
    feedback: ['3op correct', '3op1 feedback \\(\\sqrt{x}\\)', '3op2 feedback $\\sqrt{x}$', '3op3 feedback'],
    points: [100,30,0,0]
})



addcloze_numerical('clozeform1', '#q2_input_number', {
    options: [120.1, 130.3], 
    tolerance: [0.1, 1],
    points: [100, 50, 0],
    feedback: ['num op correct', 'num partial correct', 'num wrong answer']
})
*/
