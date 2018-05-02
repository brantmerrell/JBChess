\name{scrape.chess.com}
\alias{scrape.chess.com}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to scrape game data from chess.com ~~
}
\description{
%%  Takes a game id and 1-3 game types, scrapes associated data from chess.com, and returns a data frame of the results~~
}
\usage{
scrape.chess.com(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
%%     ~~a one-to-ten-digit number within the url specific to the game, ie. https://www.chess.com/games/view/<id>, https://www.chess.com/daily/game/<id>, or https://www.chess.com/live/game/<id>.~~
  \item{type}{
%%     ~~the game type in the url. If type regex-matches "open" (case ignored), function scrapes from https://www.chess.com/games/view/<id>, If type regex-matches "corres|turn|daily" (case ignored), function scrapes from https://www.chess.com/daily/game/<id>, If type regex-matches "live|game", function scrapes from https://www.chess.com/live/game/<id>. ~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~A data frame
%%  Columns: all variables scraped from 1-3 game urls
%%  Rows: 1-3 games
%% ...
}
\references{
%% ~[pgn format](https://en.wikipedia.org/wiki/Portable_Game_Notation) ~
}
\author{
%%  ~~brantmerrell~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- game_data <- scrape.chess.com(1) ----
##-- game_data <- rbind(game_data, scrape.chess.com(2))

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
