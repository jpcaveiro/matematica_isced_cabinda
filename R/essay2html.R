
# See header.html
# see style.css

library(stringr)

s = "${who} likes ${what}"
person = "tim"
thing = "kung pao"
stringr::str_interp(s, list(who=person, what=thing))

# value="${optioncode}" 
#    could be: value="option-a" or value="option-b" or ....
# ${exid}-comment 
#    could be: ex01-comment
# ${optiontext} 
#    could be: $\sqrt{x}+1$ is odd


radiotemplate <- "<input type=\"radio\" id=\"option${number}\" value=\"${number}\" name=${exid} onclick=\"show_answer(\'${exid}-comment\',\'${optioncomment}\',${bool})\">
<label for=\"option${number}\">${optiontext}</label>"


# cat(
#   stringr::str_interp(radiotemplate, list(
#   exid="ex01",
#   number=1,
#   optiontext="$\\sqrt{x}+1$ is odd", 
#   optioncomment="Longa explicação de porque a resposta está errada.")
# )
# )


formtemplate <- "<p>${enunciado}</p>
<form id=\"${exid}-form\">
${radionbuttons}</form>
<div id=\"${exid}-comment\"></div>
<button type=\"button\" onclick=\"reset(\'${exid}\')\">Limpar</button>
"


single.choice <- function(exid,enunciado,...) {
  
  strvec <- list(...)
  strvec_length = length(strvec) #nr de opções
  
  if (strvec_length<2) {
    return("single.choice() function: check arguments.\n")
  } else if (strvec_length %% 2 != 0) {
    return("single.choice() function: check arguments.\n")
  }
  
  opstr <- ""
  seqrandom <- sample(seq(1,strvec_length/2),size=strvec_length/2,replace=FALSE)
  for(opnum in seqrandom) {
    #see radiotemplate above
    opnew <- stringr::str_interp(radiotemplate, list(
      exid=exid, #something like "ex01"
      number=opnum,
      bool=ifelse(opnum==1,'true','false'),
      optiontext=strvec[1 + 2*(opnum-1)], #like:  "$\\sqrt{x}+1$ is odd", 
      optioncomment=strvec[2 + 2*(opnum-1)] #like: "Longa explicação de porque a resposta está errada.")
    ))
    opstr <- paste0(opstr,opnew,sep='<br/>\n')
  }
  
  formnew <- stringr::str_interp(formtemplate, list(
    exid = exid,
    enunciado = enunciado,
    radionbuttons=opstr
  ))
  
  cat(formnew)
}

# cat(
# single.choice(
#   "ex02",
#   "Quanto é $\\sqrt{4}$?",
#   "2 é a resposta","Correto. Uma curta resposta.",
#   "1 é a resposta","Incorreto. Uma longa resposta."
# )
# )

# https://www.w3schools.com/charsets/tryit.asp?deci=128073
# <span style='font-size:100px;'>&#128073;</span>


formtemplate.essay <- "<p id=\"${exid}-enunciado\">${enunciado}</p>
<p id=\"${exid}-click-palavraschave\" onclick=\"toggle_div(\'${exid}-palavras-chave-id\')\">&#x2B9F;&nbsp;Quais são as palavras-chave?<br/>
<div id=\"${exid}-palavras-chave-id\" style=\"display: none;\">${palavraschave}</div>
</p>
<p id=\"${exid}-click-resolucao\" onclick=\"toggle_div(\'${exid}-answer-id\')\">&#x2B9F;&nbsp;Ver proposta de resolução.<br/>
<div id=\"${exid}-answer-id\" style=\"display: none;\">${resolucao}</div>
</p>"


essay.question <- function(exid,enunciado,palavraschave,resolucao) {
  formnew <- stringr::str_interp(formtemplate.essay, list(
    exid = exid,
    enunciado = enunciado,
    palavraschave = palavraschave,
    resolucao = resolucao
  ))
  
  cat(formnew)
}



