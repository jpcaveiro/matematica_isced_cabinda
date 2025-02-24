
library(whisker)  # whisker.render
library(rvest) #imports read_html, html_children, html_text
#library(testit)



# ==== Essay ====



# An essay has a single body and one or more questions.


form.essay.template <- "
<b>Exercício ${exid}</b>
${essay}
"


# Old
# <p onclick=\"toggle_div(\'${exid}-${itemnum}-keywords-id\')\">&#x2B9F;&nbsp;Palavras-chave<br/>




#Retirado de <p>
# id=\"${exid}-${itemnum}-text-id\"
#<p onclick=toggle_div(\'ee_${exid}-${itemnum}-answer-id\')>&#x2B9F;&nbsp;Proposta de resolução<br/>

form.question.template <- "
<b>Alínea (${itemletter})</b>&nbsp;${question}
<input type=\"button\" onclick=\"toggle_div('id_${exid}-${itemnum}-keywords')\" value=\"&#x2B9F;\">&nbsp;Palavras-chave
<div id='id_${exid}-${itemnum}-keywords' style=\"display: none;\">${keywords}</div>
</p>
<input type=\"button\" onclick=\"toggle_div('id_${exid}-${itemnum}-answer')\" value=\"&#x2B9F;\">&nbsp;Proposta de resolução
<div id=\"id_${exid}-${itemnum}-answer\" style=\"display: none;\">${answer}</div>
</p>
"


form_essay <- function(exid,essay,question.list) {
  
  #Debug
  #cat('\nform_essay\n')
  
  essay.str <- stringr::str_interp(form.essay.template, list(
    exid = exid,
    essay = essay
  ))
  
  #Debug
  #print(essay.str)
  
  nlen <- length(question.list)
  questions.str <- ''
  for(i in 1:nlen) {
    
    question.new <- stringr::str_interp(form.question.template, list(
      exid = exid,
      itemnum = i,
      itemletter = letters[i],
      question = question.list[[i]]$question,
      keywords = question.list[[i]]$keywords,
      answer   = question.list[[i]]$answer
    ))
    
    #Debug
    #print(question.new)
    
    questions.str = c(questions.str,question.new,sep='\n')
  }

  return(cat(essay.str,questions.str,'\n'))
}



#' Extracts "question items" from a body_children structure
#' Example:
#' # question
#' Some text
#' ## keywords
#' some text eventually empty
#' ## answer
#' Text with answer
#'
#' @param body_children - html structure from ... 
#'
#' @return - a string with questions

make_essay_question <- function(body_children) {

  #Debug
  #cat('\n\nmake_essay_question()\n')
  #cat('\nhtml_children(body_children):\n')
  #print(html_children(body_children))
  
  # Result should be something like:
  #
  # html_children(body_children):
  # {xml_nodeset (6)}
  # [1] <h1>question</h1>
  # [2] <p>This is a question!!</p>
  # [3] <pre class="r"><code>hist(rnorm(10,0,1))</code></pre>
  # [4] <p><img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAABUA ...
  # [5] <div id="keywords" class="section level2">\r\n<h2>keywords</h2 ...
  # [6] <div id="answer" class="section level2">\r\n<h2>answer</h2>\r\ ...
   
  # Algoritmo baseado no exemplo acima:
  # lines [2] to [4] got question text
  # lines [5] e [6] got keywords <div> and answer <div>


  #Separa a tag "<div>...</div>" com tudo lá dentro
  #nas partes de uma questao
  question_parts <- html_children(body_children)
  
  # Global message
  message <- 'Cada alínea deve ser deve ter: "# question" seguida de "## keywords" e de "## answer"\n\n'
  
  
  
  # Get # question  
  

  #Verify
  testit::assert(message, grepl("question", html_text(question_parts[1]), ignore.case = TRUE)) 

  #Get html below "# question"
  len_question <- length( question_parts )
  question_html <- paste( question_parts[2:(len_question-2)], collapse = '\n')
  #Debug
  #cat('\nquestion_html\n')
  #print(question_html)
  
  
  
  # Get ## keywords  (first ## subsection of # question)
  
  
  keywords <- html_children(question_parts[len_question-1])
  #Debug
  #cat('\nkeywords_tag\n')
  #print(keywords)
  
  #Verify
  testit::assert(message, grepl("keywords", html_text(keywords[1]), ignore.case = TRUE))
  
  #Get html below "## keywords"
  len_html_keywords <- length( keywords )
  keywords_html <- paste( keywords[2:len_html_keywords], collapse = '\n')
  #Debug
  #cat('\nkeywords_html\n')
  #print(keywords_html)

  
  
  # Get ## answer  (second ## subsection of # question)
  
  
  answer <- html_children(question_parts[len_question])
  
  #Verify
  testit::assert(message, grepl("answer", html_text(answer[1]), ignore.case = TRUE))

  #Get html below "## answer"
  len_html_answer <- length( answer )
  answer_html <- paste( answer[2:len_html_answer], collapse = '\n')

  return( list(question=question_html,keywords=keywords_html,answer=answer_html) )
}




#' Extracts "questions" from a html file
#'
#' @param body_children - html structure from ... 
#'
#' @return - a string with questions
make_essay <- function(exid,body_children, num.questions.plus.1) {
  
  #Debug
  #cat('\n\nmake_essay()\n')
  
  #Debug
  #print(body_children)
  #
  #Results in:
  #
  #{xml_nodeset (3)}
  #[1] <div id="essay" class="section level1">\r\n<h1>essay</h1>\r\n< ...
  #[2] <div id="question" class="section level1">\r\n<h1>question</h1 ...
  #[3] <div id="question-1" class="section level1">\r\n<h1>question  

  
  #Debug
  #print(html_children(body_children[1]))
  #
  # results in
  #
  #{xml_nodeset (7)}
  #[1] <h1>essay</h1>
  #[2] <p>This is an R Markdown document. Markdown is a simple format ...
  #[3] <p>Uma imagem:</p>
  #[4] <p><img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAUk ...
  #[5] <p>When you click the <strong>Knit</strong> button a\r\n<stron ...
  #[6] <pre class="r"><code>summary(cars)</code></pre>
  #[7] <pre><code>

  essay.body.part <- html_children(body_children[1])
  nlen <- length(essay.body.part)  
  essay.html <- paste( essay.body.part[2:nlen], collapse = '\n')
  
  #Debug
  #print(essay.html)
  #
  # Results in
  # [1] "<p>This is an R Markdown document. Markdown is a simpl...
  

  
  # Extract question items ("alíneas")
  # First <h1> was "# essay" and 
  # next <h1> elements must be "# question"
  questions.list <- list()
  for(i in 2:num.questions.plus.1) {
    
    #Debug
    #print( html_children(body_children)[i] )
    
    #Debug
    #print( body_children[i] )

        
    #Make a questin ("alínea"):
    # body_children[i] define-se por uma tag <div>...</div> com toda a questão lá dentro
    questions.list[[i-1]] <- make_essay_question(body_children[i])
    
  }
  
  #Debug
  #print(all_questions)

  return( form_essay(exid,essay.html,questions.list) )
}



# ==== Starting routines ====


#' Extracts "questions" from a html file
#'
#' @param filename - an user/author written filename
#'
#' @return - a string with questions
make_question <- function(exid,filename) {
  
  #read_html is a function from rvest library
  html <- read_html(filename, encoding = "UTF-8")
  
  # Extracts section <body> ... </body>
  # body variable is <body>\n<div i ....</body>
  body <- html_elements(html, "body")
  
  # Inside <body> there are sub sections "children":
  body_children <- html_children(body)
  
  # Get type of question from first <h1> tag
  all_h1_elements <- html_elements(body_children, "h1")
  question_type = html_text( all_h1_elements[1] )
  
  # Debug
  #cat("\n\nTipo de Questão:'", question_type,"'", '\n\n', sep='')
  
  
  if ( grepl("essay", question_type, ignore.case = TRUE) ) {
    
    return(make_essay(exid, body_children, length(all_h1_elements)))
    
  } else if ( grepl("multichoice", question_type, ignore.case = TRUE) ) {
    
    return(make_multichoice(exid,body_children))
    
  } else {
    cat('   (Questão deve ser: "# essay" ou "# multichoice".\n')
    main_question_type <- "indefinido" #deixa-se para as variantes
    stop()
  }
  
}  




#' Convert an Rmd file containing questions
#' to html+javascript to be used in a Rmd file.
#'
#' @param filename_no_extension
#'
#'
#' @return A html+javascript string.
#' @export
#'
#' @examples
rmd2question <- function(exid,filename_no_extension) {
  
  tryCatch(
    {
      # Just to highlight: if you want to use more than one
      # R expression in the "try" part then you'll have to
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression
      # in case the "try" part was completed successfully
      
      #rmarkdown::render(paste(filename_no_extension,".Rmd",sep=""), output_format="html_document", quiet=T)
      rmarkdown::render(paste(filename_no_extension,".Rmd",sep=""), rmarkdown::html_fragment(), quiet=T)
      
      
      # The return value of `rmarkdown::render()` is the actual value
      # that will be returned in case there is no condition
      # (e.g. warning or error).
      # You don't need to state the return value via `return()` as code
      # in the "try" part is not wrapped inside a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      #
      #REVER estas mensagens e adaptar ao caso rmd2questions
      #
      if (grepl("Duplicate chunk label",cond)) {
        message("\nAvoid using code chunk labels. Code chunks from several files could have same name and `knitr()` does not accept two equal named code chunks. Find information about the code chunk in the next message.\n")
      }
      if (grepl("not found",cond)){
        #message("\nAn used variable has no declaration. Probably the code declared in an item, that contains that variable, must be relocated to '# code' where common code resides.\n")
        message("\nAn used variable was not found. Probably some code must be relocated to '# code' where common code resides.\n")
      }
      message(cond)
      #message(paste("URL does not seem to exist:", url))
      #message("Here's the original error message:")
      # Choose a return value in case of error
      #return(NA)
    },
    warning=function(cond) {
      #message(paste("URL caused a warning:", url))
      #message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      #return(NULL)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>'
      #message(paste("Processed URL:", url))
      #message("Some other message at the end")
    }
  )
  
  question_html <- make_question(exid,paste(filename_no_extension,".html",sep=""))
  
  cat(question_html)
}

