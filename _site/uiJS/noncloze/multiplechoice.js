// Escolha múltipla
export function show_answer(idname,message,correct) { 
    console.log("show_answer:" + message)
    document.getElementById(idname).innerHTML = message;
    //document.getElementById("demo").style.fontSize = "25px";
    if (correct) {
      document.getElementById(idname).style.color = "green";
    } else {
      document.getElementById(idname).style.color = "red";
    }
    //document.getElementById("demo").style.backgroundColor = "yellow";
}
export function reset(idnamepartial) { 
    const idnamecomment = idnamepartial+"-comment"
    const idnameform = idnamepartial+"-form"
    console.log("reset:" + idnamecomment)
    document.getElementById(idnamecomment).innerHTML = "";
    //document.getElementById("demo").style.fontSize = "25px";
    document.getElementById(idnamecomment).style.color = "black";
    //document.getElementById("demo").style.backgroundColor = "yellow";
    const form = document.getElementById(idnameform);
    const radios = form.querySelectorAll('input[type="radio"]');
    for (const radio of radios) {
        radio.checked = false;
    }
}