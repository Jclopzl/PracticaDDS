\name{doMatchingApplicationsCPE}
\alias{doMatchingApplicationsCPE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
doMatchingApplicationsCPE
}
\description{
Do matching between application names and CPEs names
}
\usage{
doMatchingApplicationsCPE(computers.entries = NULL, listado.cpe = cpes, threshold = 0.5, cache = FALSE, distributed = FALSE, root.dir = getwd(), applications.name.limit = NULL, listado.cpe.limit = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{computers.entries}{
List of the computers to analize
}
  \item{listado.cpe}{
List of the newest CPE's
}
  \item{threshold}{
Filter coef column taking into an account the threshols value
}
  \item{cache}{
If use cache, load cached objects
}
  \item{distributed}{
Only to split work into many workers
}
  \item{root.dir}{
Path to root
}
  \item{applications.name.limit}{
Get the names of the computer's applications
}
  \item{listado.cpe.limit}{
Limit of the CPE list (if exists)
}
}
\examples{
computers.entries.cpes <- doMatchingApplicationsCPE(
  computers.entries=computers.entries,
  listado.cpe=cpes,
  threshold=0.5,
  cache=TRUE,
  distributed=FALSE,
  applications.name.limit=0, 
  listado.cpe.limit=0
)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
