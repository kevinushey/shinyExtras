#' Make HTML Elements
#' 
#' Creates a function that returns a function that can be used to generate
#' HTML elements. See examples for usage.
#' 
#' This function returns a function that can be called as an HTML tag
#' generating function. For example, by calling
#' \code{p <- makeHTMLTag("p")}, we can generate a function that interprets
#' all named arguments as attributes, and all unnamed arguments as
#' 'data', which is generated for a \code{p} HTML tag.
#' 
#' @export
#' @param tag the HTML tag to use.
#' @param ... a collection of named and unnamed arguments;
#'  named arguments are parsed as attributes of the tag,
#'  unnamed arguments are pasted together into the inner data of the tag.
#' @seealso \code{\link{html}}
#' @examples
#' div <- makeHTMLTag("div")
#' my_class = "orange"
#' x <- "some text"
#' div( class=my_class, id="hello", "This is ", x )
makeHTMLTag <- function(tag, ...) {
  
  ## force tag into a closure
  force(tag)
  
  return( function(...) {
    
    ## get dot args
    dotArgs <- list(...)
    
    ## process attributes from named arguments
    preparsedArgs <- as.list( match.call(expand.dots=FALSE)$`...` )
    namedArgs <- dotArgs[ names( preparsedArgs ) != "" ]
    
    if( length( namedArgs ) > 0 ) {
      attrs <- paste( sep="", " ", paste( names(namedArgs), 
                                          paste( sep="",  "'",
                                                 unlist( namedArgs ),
                                                 "'" ),
                                          sep = "=", collapse = " " ) )
    } else {
      attrs <- NULL
    }
    
    ## process data from unnamed arguments
    if( is.null( names( preparsedArgs ) ) ) {
      unnamedArgs <- dotArgs
    } else {
      unnamedArgs <- dotArgs[ names(preparsedArgs) == "" ]
    }
    
    data <- paste( unnamedArgs, sep="", collapse="" )
    
    if( length( unnamedArgs ) == 0 ) {
      out <- paste( "<", tag, attrs, " />", sep="", collapse="" )
    } else {
      out <- paste( "<", tag, attrs, ">", data, "</", tag, ">",
                    sep="", collapse="")
    }
    
    class(out) <- "kHTML"
    return(out)
    
  } )
  
}

#' Print HTML Elements
#' 
#' Use this function to output HTML code for use in R Markdown documents
#' or otherwise.
#' 
#' @param ... A set of HTML tag functions. See examples for details.
#' @param file Location to output the generated HTML.
#' @export
#' @seealso
#' \code{\link{makeHTMLTag}}, for making your own tags.
#' @examples
#' html(
#'   h1("Welcome!"),
#'   div(class="header", table( tr( td("nested elements are ok") ) ) ),
#'   footer(class="foot", "HTML5 footer")
#' )
html <- function(..., file="") {
  dotArgs <- match.call(expand.dots=FALSE)$`...`
  for( item in dotArgs ) {
    print( eval( item, envir=.html ), file=file )
    cat( "\n", file=file )
  }
}

#' Print kHTML Objects
#' 
#' By default, we \code{cat} out kHTML objects as we typically
#' intend to embed them in R Markdown documents. This is mainly used for
#' printing of items in the environment \code{html}.
#' @param ... a set of kHTML objects (strings).
#' @method print kHTML
#' @S3method print kHTML
#' @seealso \code{\link{html}}
#' @examples
#' shinyExtras:::.html$br()
print.kHTML <- function(...) {
  cat( ... )
}

# Generate HTML tag environment
# 
# Generates the environment used for the HTML utility functions.
.html <- new.env()
.tags <- c(
  "a", "abbr", "acronym", "address", "applet", "area", "article", 
  "aside", "audio", "b", "base", "basefont", "bdi", "bdo", "big", 
  "blockquote", "body", "br", "button", "canvas", "caption", "center", 
  "cite", "code", "col", "colgroup", "command", "datalist", "dd", 
  "del", "details", "dfn", "dir", "div", "dl", "dt", "em", "embed", 
  "fieldset", "figcaption", "figure", "font", "footer", "form", 
  "frame", "frameset", "h1", "h2", "h3", "h4", "h5", "h6", "head", 
  "header", "hgroup", "hr", "html", "i", "iframe", "img", "input", 
  "ins", "isindex", "kbd", "keygen", "label", "legend", "li", "link", 
  "map", "mark", "menu", "meta", "meter", "nav", "noframes", "noscript", 
  "object", "ol", "optgroup", "option", "output", "p", "param", 
  "pre", "progress", "q", "rp", "rt", "ruby", "s", "samp", "script", 
  "section", "select", "small", "span", "strike", "strong", "style", 
  "sub", "summary", "sup", "table", "tbody", "td", "textarea", 
  "tfoot", "th", "thead", "time", "title", "tr", "track", "tt", 
  "u", "ul", "var", "video", "wbr"
)

for( tag in .tags ) {
  assign( tag, makeHTMLTag(tag), envir=.html )
}

rm(.tags)
