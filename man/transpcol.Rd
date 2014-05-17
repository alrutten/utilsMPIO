\name{transpcol}
\alias{transpcol}

\title{
set transparency for colors
}
\description{
converts color name to rgb and sets the transparency to the specified alpha value.
}
\usage{
transpcol(col = "red", newalpha = 100, mcv = 255)
}

\arguments{
  \item{col}{
a valid colour name
}
  \item{newalpha}{
transparency value, ranges from 0 (fully transparent) to 255 (fully opaque)
}
}
\value{
 A character vector with elements of 7 or 9 characters, ‘"#"’
     followed by the red, blue, green and optionally alpha values in
     hexadecimal (after rescaling to ‘0 ... 255’).  The optional alpha
     values range from ‘0’ (fully transparent) to ‘255’ (opaque).

}
\author{
AR
}

\seealso{
rgb(), col2rgb(), colors() 
}
\examples{

x    = c(1:255)
d    = data.frame(x = x, col =transpcol('red',newalpha = x))
histogram(d$x,col = d$col, nint = 255)

}
