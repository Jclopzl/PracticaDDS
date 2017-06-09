\name{doMatchingApplicationsCVE}
\alias{doMatchingApplicationsCVE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
doMatchingApplicationsCVE
}
\description{
For each CPE of a computer, found the related CVE using current calculated cpes and official cves list
}
\usage{
doMatchingApplicationsCVE(computers.entries.cpes, nist.entries.cves, cache = FALSE, root.dir = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{computers.entries.cpes}{
application of cumputers with CPE identifier
}
  \item{nist.entries.cves}{
Official CVE list from nist
}
  \item{cache}{
Do not do any calculation, use precalculated data
}
  \item{root.dir}{
Path to root
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
computers.entries.cves <- doMatchingApplicationsCVE(computers.entries.cpes, cves, cache=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
