\name{doMatchingApplicationsCPE}
\alias{doMatchingApplicationsCPE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{doMatchingApplicationsCPE
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
doMatchingApplicationsCPE(computers.entries = NULL, listado.cpe = cpes, threshold = 0.5, cache = FALSE, distributed = FALSE, root.dir = getwd(), applications.name.limit = NULL, listado.cpe.limit = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{computers.entries}{
%%     ~~Describe \code{computers.entries} here~~
}
  \item{listado.cpe}{
%%     ~~Describe \code{listado.cpe} here~~
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
}
  \item{cache}{
%%     ~~Describe \code{cache} here~~
}
  \item{distributed}{
%%     ~~Describe \code{distributed} here~~
}
  \item{root.dir}{
%%     ~~Describe \code{root.dir} here~~
}
  \item{applications.name.limit}{
%%     ~~Describe \code{applications.name.limit} here~~
}
  \item{listado.cpe.limit}{
%%     ~~Describe \code{listado.cpe.limit} here~~
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
test <- doMatchingApplicationsCPE(computers.entries = NULL, listado.cpe = cpes, threshold = 0.5,
    cache = FALSE, distributed = FALSE, root.dir = getwd(), applications.name.limit = NULL, 
    listado.cpe.limit = NULL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
