
var bodyHTML = document.body.innerHTML;

bodyHTML = bodyHTML.replace( /\\begin\{code\}/g, "<pre class='code'>" );
bodyHTML = bodyHTML.replace( /\\end\{code\}/g  , "</pre>" );

document.body.innerHTML = bodyHTML;
