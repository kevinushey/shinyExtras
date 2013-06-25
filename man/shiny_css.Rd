\name{shiny_css}
\alias{shiny_css}
\title{Include a CSS File}
\usage{
  shiny_css(file)
}
\arguments{
  \item{file}{Location of the file.}
}
\description{
  This function produces a singleton for including a CSS
  Stylesheet. Note that CSS files to be included in a Shiny
  server should be in the \code{www} folder; preferably
  \code{www/css}.
}

