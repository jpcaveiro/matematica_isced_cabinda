library(stringr)
library(htmltools)



CLOZE_PATTERN <- '\\{(\\d*):([^:]+):([^}]+)\\}'

#AN HTML document cannot have repetition of id labels:
INSTRUCTION_ID_LABEL_COUNTER <- 1


#cloze_match <- str_match_all(cloze_text, CLOZE_PATTERN)[[1]]
#cloze_match <- str_match_all("cloze_text", CLOZE_PATTERN)[[1]]

get_cloze_type <- function(tag) {
  
  #is done before
  #tag <- toupper(tag)

  if (tag %in% c("SHORTANSWER", "SA",  "MW")) {
    
    # short answers (SHORTANSWER or SA or MW), case is unimportant,
    tag <- "SHORTANSWER"
    
  } else if (tag %in% c("SHORTANSWER_C", "SAC", "MWC")) {
    
    # short answers (SHORTANSWER_C or SAC or MWC), case must match,
    tag <- "SHORTANSWER_C"
    
  } else if (tag %in% c("NUMERICAL", "NM")) {
    
    # numerical answers (NUMERICAL or NM),
    tag <- "NUMERICAL"
    
  } else if (tag %in% c("MULTICHOICE", "MC")) {
    
    # multiple choice (MULTICHOICE or MC), represented as a dropdown menu in-line in the text,
    tag <- "MULTICHOICE"
    
  } else if (tag %in% c("MULTICHOICE_V", "MCV")) {
    
    # multiple choice (MULTICHOICE_V or MCV), represented as a vertical column of radio buttons, or
    tag <- "MULTICHOICE_V"
    
  } else if (tag %in% c("MULTICHOICE_H", "MCH")) {
    
    # multiple choice (MULTICHOICE_H or MCH), represented as a horizontal row of radio-buttons,
    tag <- "MULTICHOICE_H"
    
  } else if (tag %in% c("MULTIRESPONSE", "MR")) {
    
    # multiple choice (MULTIRESPONSE or MR), represented as a vertical row of checkboxes
    tag <- "MULTIRESPONSE"
    
  } else if (tag %in% c("MULTIRESPONSE_H", "MRH")) {
    
    # multiple choice (MULTIRESPONSE_H or MRH), represented as a horizontal row of checkboxes
    tag <- "MULTIRESPONSE_H"
    
  } else if (tag %in% c("MULTICHOICE_S", "MCS")) {
    
    # multiple choice (MULTICHOICE_S or MCS), represented as a dropdown menu in-line in the text,
    tag <- "MULTICHOICE_S"
    
  } else if (tag %in% c("MULTICHOICE_VS", "MCVS")) {
    
    # multiple choice (MULTICHOICE_VS or MCVS), represented as a vertical column of radio buttons, or
    tag <- "MULTICHOICE_VS"
    
  } else if (tag %in% c("MULTICHOICE_HS", "MCHS")) {
    
    # multiple choice (MULTICHOICE_HS or MCHS), represented as a horizontal row of radio-buttons.
    tag <- "MULTICHOICE_HS"
    
  } else if (tag %in% c("MULTIRESPONSE_S", "MRS")) {
    
    # multiple choice (MULTIRESPONSE_S or MRS), represented as a vertical row of checkboxes
    tag <- "MULTIRESPONSE_S" 

  } else if (tag %in% c("MULTIRESPONSE_HS", "MRHS")) {
    
    # multiple choice (MULTIRESPONSE_HS or MRHS), represented as a horizontal row of checkboxes
    tag <- "MULTIRESPONSE_HS" 
    
  } else {
    
    tag <- "no tag"
    
  }
  
  return(tag)
}



parse_one_option <- function(opt_text, instruction_type) {

  #answer <- ""
  #feedback <- ""
  
  
  #get percent valid
  if (substr(opt_text,1,1) == '=') {
    
    percent <- "100"
    opt_text <- substr(opt_text, 2, nchar(opt_text))
  
  } else if (substr(opt_text,1,1) == '%') {
    
    percent_match <- str_match(opt_text, "^%([-+]?\\d+)%(.+)")
    #percent_match <- percent_match[[1]]
    
    
    num_of_matches <- length(percent_match) #/ 4
    
    if (num_of_matches > 0) {
      percent  <- percent_match[[1,2]]
      opt_text <- percent_match[[1,3]]
    } else {
      percent <- "0"
    }
  } else {
    
    percent <- "0"
    
  }
  

  #Find feedback
  text_hash_feedback <- strsplit(opt_text, '#')[[1]]
  if (length(text_hash_feedback) == 1) {
    
    #No hastag 
    answer <- text_hash_feedback
    feedback <- ""
    
  } else if (length(text_hash_feedback) == 2) {
    
    #answer and feedback
    answer   <- text_hash_feedback[1]
    feedback <- text_hash_feedback[2]
    
  } else {   
    
    #probabl feedback contains hastag
    #TODO: possible error
    answer <- text_hash_feedback[1]
    feedback <- paste(text_hash_feedback[2:length(text_hash_feedback)], collapse = "#")
  }
  
  
  if (instruction_type=="NUMERICAL") {
    
    # Catch tolerance
    ans_parts <- text_hash_feedback <- strsplit(opt_text, ':')[[1]] #a list of one element that is a vector
    if (length(ans_parts)==1) {
      o = list("percent"=percent, "answer"=ans_parts[1], "feedback"=feedback, "tolerance"=0)  
    } else if (length(ans_parts)==2) {
      o = list("percent"=percent, "answer"=ans_parts[1], "feedback"=feedback, "tolerance"=ans_parts[2])  
    } else {
      stop("cloze: NUMERICAL option must be number:tolerance, at most once, and not other ':'.\n")  # Raise an error
    }
    
  } else {
    
    o = list("percent"=percent, "answer"=answer, "feedback"=feedback)  
    
  }
  
  

  return(o)
}




instruction2explicitclozeformat <- function(cparts) {
  #cparts is short for cloze_question_parts
  #cloze_question_parts <- list("points"=question_points, "type"=question_type, "options"=list_options)
  
  options_text <- c()
  for (o in cparts$options) {
    #o = list("percent"=percent, "answer"=answer, "feedback"=feedback)
    options_text <- c(options_text, paste0("%",o$percent,"%",o$answer,"#",o$feedback,collapse=""))
  } 
  options_text <- paste0(options_text,collapse="~")
  
  return(paste0("{",cparts$points,":",cparts$type,":",options_text))
}



#' instruction2html
#'
#' @param cl - like `list("points"=question_points, "type"=question_type, "options"=list_options)`
#' @param cloze_number
#'
#' @returns
#' @export
#'
#' @examples
instruction2html <- function(cparts, cloze_number) {
  
  # cparts is like: 
  #   list("points"=question_points, "type"=question_type, "options"=list_options)
  # and
  # each option in list_options is like
  #   list("percent"=percent, "answer"=answer, "feedback"=feedback)
  
  # Ver C:\Users\pedrocruz\Documents\GitHub\estatisticaaplicada\bookdown\interaction.R
  
  
  #Extract question_type
  question_type <- cparts["type"]
  
  #New html id
  # Como gerar labels div id="q1_dropdown" ou id="q2_input_number" 
  # que não se podem repetir num documento HTML.
  instruction_id <- paste0("q", INSTRUCTION_ID_LABEL_COUNTER, "_", question_type)
  INSTRUCTION_ID_LABEL_COUNTER <<- INSTRUCTION_ID_LABEL_COUNTER + 1

  
  # list_options is like: list("percent"=percent, "answer"=answer, "feedback"=feedback)

  #Extract each option answer
  if (question_type == "NUMERICAL") {
    
    #Textual answers
    answers <- sapply(cparts[["options"]], function(x) as.numeric(x[["answer"]]))
    answers_html <- paste0("[",paste(answers, collapse=", "),"]")
  
    #Extract each option feedbacks
    tolerances <- sapply(cparts[["options"]], function(x) as.numeric(x[["tolerance"]]))
    tolerances_html <- paste0("[",paste(tolerances, collapse=", "),"]")

    #Extract each option points
    points <- sapply(cparts[["options"]], function(x) as.integer(x[["percent"]]))
    #add 0 points when user inputs a value that is not on the list
    points_html <- paste0("[",paste(points, collapse=", "),", 0]") 
    

    #Extract each option feedbacks
    feedbacks <- sapply(cparts[["options"]], function(x) paste0("'",x[["feedback"]],"'"))
    feedbacks_html <- paste0("[",paste(feedbacks, collapse=", "),", 'incorreto']")
  
  } else {
    
    #Textual answers
    answers <- sapply(cparts[["options"]], function(x) paste0("'",x[["answer"]],"'"))
    answers_html <- paste0("[",paste(answers, collapse=", "),"]")
    
    #Extract each option points
    points <- sapply(cparts[["options"]], function(x) as.integer(x[["percent"]]))
    points_html <- paste0("[",paste(points, collapse=", "),"]")
  
    #Extract each option feedbacks
    feedbacks <- sapply(cparts[["options"]], function(x) paste0("'",x[["feedback"]],"'"))
    feedbacks_html <- paste0("[",paste(feedbacks, collapse=", "),"]")
    
  }
  

  # ======= HTML ==============
  # addcloze_multichoice_s('clozeform1', '#q3_dropdown', {
  #   options: ['3uma opção correta', '3w.op1', '3w.op2', '3wop3'], 
  #   feedback: ['3op correct', '3op1 feedback \\(\\sqrt{x}\\)', '3op2 feedback $\\sqrt{x}$', '3op3 feedback'],
  #   points: [100,30,0,0]
  # })
  # 
  # 
  # 
  # addcloze_numerical('clozeform1', '#q2_input_number', {
  #   options: [120.1, 130.3], 
  #   tolerance: [0.1, 1],
  #   points: [100, 50, 0],
  #   feedback: ['num op correct', 'num partial correct', 'num wrong answer']
  # })
  # 
  # <form name="clozeform1">
  #   <span>
  #   Muito texto <div id="q1_dropdown" class="clozeitem"></div> e continuação de muito 
  #   texto com <div id="q2_input_number" class="clozeitem"></div> e mais texto.</br>
  #     Depois, responda a isto: <div id="q3_dropdown" class="clozeitem"></div>.
  #   </span>
  # </form>
  #     
      
  
  
  if (question_type == "MULTICHOICE_S") {
    
    multichoice_s_template <- "
addcloze_multichoice_s(\'${clozeformname}\', \'#${instruction_id}\', {
 options: ${options}, 
 feedback: ${feedbacks},
 points: ${points}
});
"
    #Debug
    #print(answers_html)
    #print(points_html)
    #print(feedbacks_html)
    
    instruction_str <- stringr::str_interp(
      multichoice_s_template,
      list(instruction_id=instruction_id,
           options=answers_html,
           points=points_html,
           feedbacks=feedbacks_html,
           clozeformname=paste0("clozeform_",cloze_number)
      )
    )
      
    
  } else if (question_type == "SHORTANSWER") {

    #TODO: adicionar case
    shortanswer_template <- "
addcloze_shortanswer(\'${clozeformname}\', \'#${instruction_id}\', {
 options: ${options}, 
 feedback: ${feedbacks},
 points: ${points}
});
"
    #Debug
    #print(answers_html)
    #print(points_html)
    #print(feedbacks_html)
    
    instruction_str <- stringr::str_interp(
      shortanswer_template,
      list(instruction_id=instruction_id,
           options=answers_html,
           points=points_html,
           feedbacks=feedbacks_html,
           clozeformname=paste0("clozeform_",cloze_number)
      )
    )
    
  } else if (question_type == "NUMERICAL") {
    
    #TODO: adicionar case
    numerical_template <- "
addcloze_numerical(\'${clozeformname}\', \'#${instruction_id}\', {
 options: ${options}, 
 feedback: ${feedbacks},
 points: ${points}, 
 tolerances: ${tolerances}
});
"
    #Debug
    #print(answers_html)
    #print(points_html)
    #print(feedbacks_html)
    
    instruction_str <- stringr::str_interp(
      numerical_template,
      list(instruction_id=instruction_id,
           options=answers_html,
           points=points_html,
           feedbacks=feedbacks_html,
           tolerances=tolerances_html,
           clozeformname=paste0("clozeform_",cloze_number)
      )
    )
    
  }
  
  html_instruction_script <- stringr::str_interp(
    "<div id=\'${instruction_id}\' class='clozeitem'></div>\n",
    list(instruction_id=instruction_id)
  )
  
  return(c(html_instruction_script, instruction_str))
  
}
  


#' Parse one cloze instruction 
#' 
#' Instructions are like {:SHORTANSWER:=Paris#Correct!~*#Wrong answer}
#' and breaks them into its parts.
#'
#' @param cloze_instruction - instructions has here ...moodle cloze page....
#' @param output - see line: `if (output=="cloze") {``
#'
#' @returns
#' @export
#'
#' @examples
#' \dontrun{cloze_instruction <- "{:SHORTANSWER:=Paris#Correct!~*#Wrong answer}"}
#' \dontrun{print(parse_one_cloze_instruction(cloze_instruction, output="html"))}
#' \dontrun{cloze_instruction <- "{1:MCV:%50%Jupiter#Partial Correct!~=Mars#Correct~Saturn#Wrong}"}
#' \dontrun{print(parse_one_cloze_instruction(cloze_instruction, output="html"))}
parse_one_cloze_instruction <- function(cloze_instruction, cloze_number, output="cloze") {
  #    The function only checks the syntax of CLOZE expressions.

  #cloze_match <- str_match_all(cloze_instruction, CLOZE_PATTERN)[[1]]
  cloze_match <- str_match(cloze_instruction, CLOZE_PATTERN)
  
  if (length(cloze_match) / 4 != 1) {
    return(paste("parse_cloze_instruction: function must be applied to a single cloze instruction. Check", cloze_instruction,"\n", colappse=""))
  }
  
  m <- 1 # only match
  
  question_fullstr <- cloze_match[m,1]
  question_points <- cloze_match[m,2] #poins
  if (question_points == "") {
    question_points <- "1"
  }
  question_type   <- toupper(cloze_match[m,3]) #instruction
  question_content<- cloze_match[m,4] #configuration
  

  tag <- get_cloze_type(question_type)
  if (tag == "no tag") {
    cat("CLOZE ERROR: ", question_fullstr, "\n")
    cat("  Instruction '", question_type, "' does not exist (please, check spelling).\n", sep="")
    next
  } else {
    question_type <- tag
  }

  #Debug
  #cat("CLOZE: ", cloze_match[m,1], "\n")
  #cat("  Points: ", question_points, "\n")
  #cat("  Type: ", question_type, "\n")
  #cat("  Content: ", question_content, "\n")

      
  # #Normalize use of: ~, =, %100%, etc.
  # #=resposta correta~incorreta#feedback
  # #Text should not contain those symbols, yet.
  options <- strsplit(question_content, "~")[[1]]
  
  if (length(options) == 0) {
    cat("CLOZE Error: question '", question_fullstr, "without options.\n", sep="")
    next
  }  

  list_options <- c()
  for (opt_text in options) { 


    #Debug
    #print(opt_text)
    
    #TODO: protect from mistakes like "<no text>#Some feedback" 
    o <- parse_one_option(opt_text, instruction_type=tag)
    
    list_options <- append(list_options, list(o))
  }
  
  #Verifica se há 100 em alguma a opção :
  # tot = [o.percent>=100 for o in all_options]
  tot  <- sapply(list_options, function(o) o["percent"] >= 100)

  if (!any(tot)) {
    cat("CLOZE ERROR: in '", question_fullstr, "' there is no correct option.\n", sep="")
    next
  }
  
  instruction_parts <- list("points"=question_points, "type"=question_type, "options"=list_options)
  #Debug
  #print(q)


  
  if (output=="originalcloze") {
    return(cloze_match[m,1])
  } else if (output=="explicitclozeformat") {
    return(instruction2regexcloze(instruction_parts))
  } else if (output=="html") {
    return(instruction2html(instruction_parts, cloze_number))
  } else {
    return(instruction_parts)
  }
}
 




#' Transform text written in cloze format intro something else.
#' 
#' @param cloze_text 
#' @param cloze_number
#' @param output 
#'
#' @returns
#' @export
#'
#' @examples
cloze_transform <- function(cloze_text, cloze_number=1, output="originalcloze") {
  matches_info <- gregexpr(CLOZE_PATTERN, cloze_text, perl = TRUE)[[1]]
  
  #print(matches_info[1])
  #print(attr(matches_info,"match.length"))
  
  
  n_clozematches <- length(matches_info)
  cloze_start    <- c(matches_info)
  cloze_len      <- c(attr(matches_info,"match.length"))
  
  
  if (output=="html") {
    html_cloze_script <- paste(
      "<script type='module'>",
      "/*check cloze_transform funtion in R*/",
      "import {addcloze_multichoice_s} from './uiJS/cloze/multichoice.js';",
      "import {addcloze_numerical} from './uiJS/cloze/numerical.js';",
      "import {addcloze_shortanswer} from './uiJS/cloze/shortanswer.js';",
      "\n",
      sep="\n")

  }
  
  
  
  cloze_text_parts <- c()
  
  text_pos <- 1
  
  # Situations:
  #
  # 1. text{instruction}text{instruction}text
  # 2. {instruction}text{instruction}text
  # 3. text{instruction}text{instruction}
  # 4. {instruction}{instruction}
  
  
  for(m in 1:n_clozematches) {
    
    if (text_pos < cloze_start[m]) {
      cloze_text_parts <- c(cloze_text_parts, substr(cloze_text, text_pos, cloze_start[m]-1))
    } else {
      #não é adicionado texto porque text_pos = cloze_start
    }
    
    # call parse_one_cloze_instruction()
    #TODO: fazer para outros outputs
    
    if (output == "I") {
      
      instruction <- paste(rep("I",cloze_len[m]),collapse="")
      
    } else if (output=="html") {
      
      original_instruction <- substr(cloze_text, cloze_start[m], cloze_start[m]+cloze_len[m]-1)
      
      instruction_pair <- parse_one_cloze_instruction(original_instruction, cloze_number, output)
      
      instruction <- instruction_pair[1] #<div id=q1_multichoice_s></div>
      html_cloze_script <- c(html_cloze_script, instruction_pair[2]) #js call
      
    }
    
    cloze_text_parts <- c(cloze_text_parts, instruction)
    
    #Prepare next
    text_pos <- cloze_start[m] + cloze_len[m]
  }
  
  cloze_text_length <- nchar(cloze_text)
  if (text_pos<=cloze_text_length) {
    cloze_text_parts <- c(cloze_text_parts, substr(cloze_text, text_pos, cloze_text_length))
  }
  
  
  
  if (output=="html") {
    html_cloze_script_txt <- paste0(paste0(html_cloze_script, collapse=""), "</script>\n", collapse="\n")
    cloze_text_parts <- paste0(html_cloze_script_txt, paste0(cloze_text_parts, collapse=""), collapse="\n")
  }
  
  
  
  #A vector of text and instructions is returned.
  return(cloze_text_parts)
}




#' cloze
#'
#' @param cloze_text 
#' @param cloze_number - in html page, forms must have different identifiers
#' @param output 
#'
#' @returns
#' @export
#'
#' @examples
cloze <- function(cloze_text, cloze_number=1, output="html") {

  #Convert from "md" to "html":
  cloze_html <- pandoc::pandoc_convert(
         text = cloze_text,
         from = "markdown",
         to = "html"
  )
   
  
  cloze_html <- paste(cloze_html, collapse="\n")
  
  #debug
  #cat(class(cloze_html),"\n")
  #cat(length(cloze_html),"\n")
  
  cloze_text_parts <- cloze_transform(cloze_html, cloze_number, output)
  cloze_text <- paste0(cloze_text_parts, collapse = "")

  if (output=="originalcloze") {
    
    #Return the original author string
    cat(cloze_text,"\n")
    
  } else if (output=="html") {
    
    #debug
    #cat(paste0("HTML text:\n", cloze_text, "\n"))
    
    HTML(cloze_text)
    
  } else if (output=="explicitclozeformat") {
    
    cat(cloze_text,"\n")
    
  } else {
    
    return(cloze_text)
    
  }

}

#debug(instruction2html)

             #0        1         2         3         4         5         6         7         8         9        10        11        12        13        14        15        16        17
             #12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
#cloze_text = "The largest planet in the solar system is {1:MULTICHOICE_S:%50%Jupiter#Partial Correct!~=Mars#Correct~Saturn#Wrong}. The capital of France is {:SHORTANSWER:=Paris#Correct!~*#Wrong answer}. "
#print(cloze(cloze_text, cloze_number=1, output="html"))

