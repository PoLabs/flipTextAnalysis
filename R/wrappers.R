# Wrapper function for package tm's Term Matrix Construction
# Returns a matrix with documents in rows, and terms in columns.
termMatrixFromText = function(text, min.frequency = 5, sparse = FALSE) {
  source = tm::VectorSource(text)
  corpus = tm::VCorpus(source)
  my.tdm = tm::DocumentTermMatrix(corpus)
  my.tdm = tm::weightBin(my.tdm)
  # Get words which appear with certain frequency
  # and generate term matrix based on those words
  my.dictionary = tm::findFreqTerms(my.tdm, lowfreq = min.frequency)
  my.tdm = tm::DocumentTermMatrix(corpus, list(dictionary = my.dictionary))
  my.tdm = tm::weightBin(my.tdm)
  if (!sparse)
      my.tdm <- as.matrix(my.tdm)
  return(invisible(my.tdm))
}


#' \code{AsTermMatrix}
#'
#' @description Generate a matrix of sentiment scores from a character vector or wordBag object.
#'
#' @param x Either a character vector or a WordBag object, created for example by the function InitializeWordBag.
#'          If \code{x} is a character vector then additional parameters can be supplied for processing the
#'          text before scoring sentiment. For a description of the options see \code{\link{InitializeWordBag}}.
#' @param sparse Whether to return the term matrix as a sparse matrix.
#' @inheritParams InitializeWordBag
#'
#' @return  A \code{matrix} with one row for each text response (referred to as a \code{document}) and once column for
#'          each word or phrase extracted from the text.
#'
#' @examples
#' AsTermMatrix(ftaFavoriteThings)
#'
#' @export
AsTermMatrix = function(x, min.frequency = 5, remove.stopwords = TRUE, stoplist = ftaStopList,
  operations = c("spelling", "stemming"), spelling.dictionary = ftaDictionary,
  manual.replacements = NULL, sparse = FALSE)
{
  if (class(x) == "wordBag") {
    tdm <- termMatrixFromText(x$transformed.text, min.frequency = min.frequency, sparse = sparse)
  } else if (class(x) == "character") {
    word.bag <- InitializeWordBag(x, remove.stopwords = remove.stopwords, stoplist = stoplist, operations = operations,
                                spelling.dictionary = spelling.dictionary, manual.replacements = manual.replacements)
    tdm <- termMatrixFromText(word.bag$transformed.text, min.frequency = min.frequency, sparse = sparse)
  } else if (class(x) == "tidyText") {
    tdm <- termMatrixFromText(x, min.frequency = min.frequency, sparse = sparse)
  }
  colnames(tdm) <- gsub("\\+", ".", colnames(tdm))
  return(tdm)
}


#' \code{AsSentimentMatrix}
#' @description Generate a matrix of sentiment scores from a character vector or wordBag object.
#'
#' @param x Either a character vector or a WordBag object, created for example by the function InitializeWordBag.
#'          If \code{x} is a character vector then additional parameters can be supplied for processing the
#'          text before scoring sentiment. For a description of the options see \code{\link{InitializeWordBag}}.
#' @param pos.words A character vector containing words that should be scored positively.
#' @param neg.words A character vector containing words that should be scored negatively.
#' @inheritParams InitializeWordBag
#'
#' @return A matrix with three columns, and one row for each of the original text responses in the word bag.
#'         The first column provides a count of the positive words that have been identified in each
#'         response, the second column gives a count of the negative words, and the third column gives the
#'         net sentiment score, which is the difference between the two.
#'
#' @details The scoring algorithm attempts to handle negation by looking for words like "not" which occur
#'          one or two tokens prior to a sentiment word. For example, "not bad" would yield a positive
#'          score of 1 rather than a negative score of -1 which would be applied for the word "bad".
#'
#' @examples
#' AsSentimentMatrix(ftaFavoriteThings)
#' @export
AsSentimentMatrix <- function(x, remove.stopwords = TRUE, stoplist = ftaStopList,
  operations = c("spelling", "stemming"), spelling.dictionary = ftaDictionary,
  manual.replacements = NULL, pos.words = ftaPositiveWords,
  neg.words = ftaNegativeWords)
{

  .sentimentScoresFromWordBag <- function(word.bag, pos.words = ftaPositiveWords, neg.words = ftaNegativeWords)
  {
      tagged.text <- TagSentiment(word.bag$tokens, pos.words, neg.words)
      sentiment.scores <- lapply(word.bag$transformed.text, ScoreSentimentForString, tokens = word.bag$tokens, sentiment.tags = tagged.text)
      sentiment.matrix <- matrix(unlist(sentiment.scores), nrow = length(sentiment.scores), byrow = TRUE)
      sentiment.matrix <- cbind(sentiment.matrix, sentiment.matrix[, 1] - sentiment.matrix[, 2])
      colnames(sentiment.matrix) <- c("Positive words", "Negative words", "Sentiment score")
      rownames(sentiment.matrix) <- word.bag$original.text
      return(sentiment.matrix)
  }

  if (class(x) == "wordBag")
  {
    word.bag <- x
  } else if (class(x) == "character") {
    word.bag <- InitializeWordBag(x, remove.stopwords = remove.stopwords, stoplist = stoplist, operations = operations,
                                 spelling.dictionary = spelling.dictionary, manual.replacements = manual.replacements)
  } else {
    stop(paste("AsSentimentMatrix: Cannot create sentiment matrix from objects of class ", class(x), ". Input must be a wordBag or character vector.", sep = ""))
  }
  sentiment.matrix <- .sentimentScoresFromWordBag(word.bag, pos.words = pos.words, neg.words = neg.words)
}


#' \code{MostFrequentWords}
#'
#' @description Count the words appearing in text.
#'
#' @param x A character vector containing the text that you want to process
#'          or an object of class \code{wordBag} created, for example, by
#'          \code{\link{InitializeWordBag}}.
#' @param min.frequency An integer specifying the minium count to keep in the output.
#' @param alphabetical A boolean value specifying whether you want the output to be
#'                     sorted alphabetically or by count.
#'
#' @return A vector of integers, where the name of each element corresponds to the word
#'         and the value corresponds to the count.
#'
#' @examples
#' MostFrequentWords(ftaFavoriteThings, min.frequency = 3)
#'
#' @export
MostFrequentWords = function(x, min.frequency = 2, alphabetical = NULL)
{
    if (class(x) == "wordBag")
    {
        tokens = x$final.tokens
        counts = x$final.counts
    } else if (class(x) == "character") {
        tokenized = ftaTokenize(x)
        tokens.counts = countUniqueTokens(tokenized)
        tokens = tokens.counts$tokens
        counts = tokens.counts$counts
    }
    if (is.null(alphabetical))
    {
        if (class(x) == "wordBag")
        {
            alphabetical = x$alphabetical.sort
        } else {
            alphabetical = FALSE
        }
    }

    tokens = tokens[counts >= min.frequency]
    counts = counts[counts >= min.frequency]

    if (alphabetical)
    {
        counts = counts[order(tokens)]
        tokens = sort(tokens)
    } else {
        tokens = tokens[order(counts, decreasing = TRUE)]
        counts = sort(counts, decreasing = TRUE)
    }

    names(counts) = makeWordBagTextReadable(tokens)
    return(counts)
}
