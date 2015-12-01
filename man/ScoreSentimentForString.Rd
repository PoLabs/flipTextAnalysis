% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/flipTextAnalytics-sentiment.R
\name{ScoreSentimentForString}
\alias{ScoreSentimentForString}
\title{\code{ScoreSentimentForString}}
\usage{
ScoreSentimentForString(string, tokens, sentiment.tags)
}
\arguments{
\item{string}{The string to evaulate.}

\item{tokens}{A vector of strings that have been identified in the text from which
\code{string} has been drawn.}

\item{sentiment.tags}{A vector of sentiment tags for the \code{tokens}, obtained
                      by \code{\link{TagSentiment}}.}
}
\value{
A vector of two integers, where the first indicates the positive sentiment
         score, and the second contains the negative sentiment score.
}
\description{
Generate scores of positive and negative sentiment for a string.
}
\details{
This function attempts to handle some negation of sentiment by looking at the
         words preceding a sentiment word and check for things like \code{"not"},
         \code{"wasnt"}, etc.
}
\examples{
my.tokens = c("im", "not", "very", "happy")
my.tags = TagSentiment(my.tokens)
ScoreSentimentForString("im not very happy",
                         tokens = my.tokens,
                         sentiment.tags = my.tags)
}
