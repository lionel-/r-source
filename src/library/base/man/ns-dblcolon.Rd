% File src/library/base/man/ns-dblcolon.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{ns-dblcolon}
\title{Double Colon and Triple Colon Operators}
\alias{::}
\alias{:::}
\description{
  Accessing exported and internal variables, i.e.\sspace{}\R objects
  (including lazy loaded data sets) in a namespace.
}
\usage{
pkg::name
pkg:::name
}
\arguments{
  \item{pkg}{package name: symbol or literal character string.}
  \item{name}{variable name: symbol or literal character string.}
}
\details{
  For a package \pkg{pkg}, \code{pkg::name} returns the value of the
  exported variable \code{name} in namespace \code{pkg}, whereas
  \code{pkg:::name} returns the value of the internal variable
  \code{name}.  The package namespace will be loaded if it was not
  loaded before the call, but the package will not be attached to the
  search path.

  Specifying a variable or package that does not exist is an error.

  Note that \code{pkg::name} does \strong{not} access the objects in the
  environment \code{package:pkg} (which does not exist until the
  package's namespace is attached): the latter may contain objects not
  exported from the namespace.  It can access datasets made available by
  lazy-loading.
}

\note{
  It is typically a design mistake to use \code{:::}
  in your code since the corresponding object has probably been kept
  internal for a good reason.  Consider contacting the package
  \code{\link{maintainer}} if you feel the need to access the object for
  anything but mere inspection.
}
\seealso{
  \code{\link{get}} to access an object masked by another of the same name.
  \code{\link{loadNamespace}}, \code{\link{asNamespace}} for more about
  namespaces.
}
\examples{
base::log
base::"+"

## Beware --  use ':::' at your own risk! (see "Details")
stats:::coef.default
}
\keyword{programming}
