#' Include a JavaScript File
#' 
#' This function produces a singleton for including a JavaScript file. Note
#' that JavaScript files to be included in a Shiny server should be in the
#' \code{www} folder; preferably \code{www/js}.
#' @param file Location of the file.
#' @importFrom shiny singleton tags
#' @export
js <- function(file) {
  if( !file.exists(file) ) {
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
css <- function(file) {
  if( !file.exists(file) ) {
    warning("No CSS stylesheet located at '", file, "'.")
  }
  return( singleton( tags$head( tags$link(
    rel="stylesheet", type="text/css", href=file
  ))))
}
