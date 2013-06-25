#' Include a JavaScript File
#' 
#' This function produces a singleton for including a JavaScript file. Note
#' that JavaScript files to be included in a Shiny server should be in the
#' \code{www} folder; preferably \code{www/js}.
#' @param file Location of the file.
#' @importFrom shiny singleton tags
#' @export
shiny_js <- function(file) {
  if( !file.exists(file.path("www", file)) ) {
    warning("No JavaScript file located at '", file, "'.")
  }
  return( singleton( tags$head( tags$script(
    type="text/javascript", src=file
  ) ) )
  )
}

#' Include a CSS File
#' 
#' This function produces a singleton for including a CSS Stylesheet.
#' Note that CSS files to be included in a Shiny server should be in the
#' \code{www} folder; preferably \code{www/css}.
#' @param file Location of the file.
#' @importFrom shiny singleton tags
#' @export
shiny_css <- function(file) {
  if( !file.exists(file.path("www", file)) ) {
    warning("No CSS stylesheet located at '", file, "'.")
  }
  return( singleton( tags$head( tags$link(
    rel="stylesheet", type="text/css", href=file
  ))))
}

#' Include D3.js
#' 
#' This function produces a singleton for including d3.js as:
#' \code{<script src="http://d3js.org/d3.v3.min.js" charset="utf-8"></script>}.
#' 
#' @importFrom shiny singleton tags
#' @export
use_d3 <- function() {
  return( singleton( tags$head( tags$script(
    type="text/javascript", charset="utf-8", src="http://d3js.org/d3.v3.min.js"
  ))))
}
