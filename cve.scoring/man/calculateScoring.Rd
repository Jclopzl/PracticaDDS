\name{calculateScoring}
\alias{calculateScoring}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculateScoring
}
\description{
Calculate an scoring for each computer
}
\usage{
calculateScoring(computers.entries.cves, computers.entries.criticity, cache = FALSE, use_simulation = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{computers.entries.cves}{
computer information with cves for each PC
}
  \item{computers.entries.criticity}{
Criticity factor for each PC, depnding on their importance for the business.
}
  \item{cache}{
Load precalculated scoring according original source
}
  \item{use_simulation}{
true if use a sample file used to test visualization
}
}
\examples{
computers.entries.scoring <- calculateScoring (computers.entries.cves, computers.entries.criticity, cache=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
