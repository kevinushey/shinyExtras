\name{print.kHTML}
\alias{print.kHTML}
\title{Print kHTML Objects}
\usage{
  \method{print}{kHTML} (...)
}
\arguments{
  \item{...}{a set of kHTML objects (strings).}
}
\description{
  By default, we \code{cat} out kHTML objects as we
  typically intend to embed them in R Markdown documents.
  This is mainly used for printing of items in the
  environment \code{html}.
}
\examples{
shinyExtras:::.html$br()
}
\seealso{
  \code{\link{html}}
}

