\name{loadComputerSoftware}
\alias{loadComputerSoftware}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
loadComputerSoftware
}
\description{
Load software inventory from all computers files included in the input.dir path.

Read all files contained into <input.dir>. Each file is a CSV file. The file format is:
  First line: contains header.
  Column 1: Name. Software or application name.
  Column 2: Version. Software versión.
  Column 3: Vendor. Company who has developed the product.

The name of the file is taken as The name of the computer.
}
\usage{
loadComputerSoftware(input.dir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input.dir}{
Path containing software computer input files
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
computers.entries <- loadComputerSoftware(INPUT_COMPUTER_DIR)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
