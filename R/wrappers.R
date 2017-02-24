# Wrapper function for package tm's Term Matrix Construction
# Returns a matrix with documents in rows, and terms in columns.
termMatrixFromText = function(text, min.frequency = 5, sparse = FALSE) {
    source <- tm::VectorSource(text)
    corpus <- tm::VCorpus(source)
    my.tdm <- tm::DocumentTermMatrix(corpus, control = list(wordLengths = c(1, Inf)))
    my.tdm <- tm::weightBin(my.tdm)
    # Get words which appear with certain frequency
    # and generate term matrix based on those words
    my.dictionary <- tm::findFreqTerms(my.tdm, lowfreq = min.frequency)
    my.tdm <- tm::DocumentTermMatrix(corpus, control = list(dictionary = my.dictionary, wordLengths = c(1, Inf)))
    my.tdm <- tm::weightBin(my.tdm)
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
AsTermMatrix = function(x,
                        min.frequency = 5,
                        remove.stopwords = TRUE,
                        stoplist = get("ftaStopList"),
                        operations = c("spelling", "stemming"),
                        spelling.dictionary = get("ftaDictionary"),
                        manual.replacements = NULL,
                        sparse = FALSE)
{
    if (class(x) == "wordBag") {
        tdm <- termMatrixFromText(x$transformed.text, min.frequency = min.frequency, sparse = sparse)
    } else if (class(x) == "character") {
        word.bag <- InitializeWordBag(x,
                                      remove.stopwords = remove.stopwords,
                                      stoplist = stoplist,
                                      operations = operations,
                                      spelling.dictionary = spelling.dictionary,
                                      manual.replacements = manual.replacements)
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
#' @inheritParams TagSentiment
#' @inheritParams ScoreSentimentForString
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
AsSentimentMatrix <- function(x,
                              remove.stopwords = TRUE,
                              stoplist = get("ftaStopList"),
                              operations = c("spelling", "stemming"),
                              spelling.dictionary = get("ftaDictionary"),
                              manual.replacements = NULL,
                              pos.words = get("ftaPositiveWords"),
                              neg.words = get("ftaNegativeWords"),
                              check.simple.suffixes = FALSE,
                              simple.suffixes = c("s", "es", "ed", "d", "ing"),
                              blanks.as.missing)
{

    .sentimentScoresFromWordBag <- function(word.bag, pos.words = get("ftaPositiveWords"), neg.words = get("ftaNegativeWords"), blanks.as.missing)
    {
        input.tokens <- countUniqueTokens(word.bag$transformed.tokenized)$tokens

        tagged.text <- TagSentiment(input.tokens,
                                    pos.words,
                                    neg.words,
                                    check.simple.suffixes = check.simple.suffixes,
                                    simple.suffixes = simple.suffixes)
        sentiment.scores <- lapply(word.bag$transformed.text, ScoreSentimentForString, tokens = input.tokens, sentiment.tags = tagged.text, blanks.as.missing = blanks.as.missing)
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
    sentiment.matrix <- .sentimentScoresFromWordBag(word.bag, pos.words = pos.words, neg.words = neg.words, blanks.as.missing = blanks.as.missing)
}


#' \code{SaveNetSentimentScores}
#' @description Return the difference between the number of positive and negative words for
#'              a character vector or wordBag object.
#' @param input Either a character vector or object of class \code{wordBag}.
#' @inheritParams TagSentiment
#' @inheritParams ScoreSentimentForString
#' @details If the input is a character vector then the text will be tokenized to compute the
#'          sentiment, but no additional cleaning is done on the text. If the input is a \code{wordBag}
#'          then the sentiment will be computed based on the transformed text (i.e. using the
#'          results of the cleaning that has been done by the wordBag).
#' @return A vector of integers showing the number of positive words minus the number of negative
#'         words for each text entry.
#' @export
SaveNetSentimentScores <- function(input,
                                   check.simple.suffixes = FALSE,
                                   simple.suffixes = c("s", "es", "ed", "d", "ing"),
                                   pos.words = get("ftaPositiveWords"),
                                   neg.words = get("ftaNegativeWords"),
                                   blanks.as.missing = FALSE)
{
    if (class(input) == 'wordBag')
    {
        sentiment.matrix <- AsSentimentMatrix(input,
                                              check.simple.suffixes = check.simple.suffixes,
                                              simple.suffixes = simple.suffixes,
                                              pos.words = pos.words,
                                              neg.words = neg.words,
                                              blanks.as.missing = blanks.as.missing)
    } else if (class(input) == 'character') {
        sentiment.matrix <- AsSentimentMatrix(input,
                                              remove.stopwords = FALSE,
                                              operations = '',
                                              check.simple.suffixes = check.simple.suffixes,
                                              simple.suffixes = simple.suffixes,
                                              pos.words = pos.words,
                                              neg.words = neg.words,
                                              blanks.as.missing = blanks.as.missing)
    } else {
        stop('The input should be created by selecting Insert > Advanced > Text Analysis > Setup.')
    }
    sentiment.scores <- sentiment.matrix[,3]
}


#' \code{SaveTidiedText}
#' @description Return the tidied text from a wordBag. This text is the result of the processing
#' done by the wordBag.
#' @param word.bag An object of class \code{wordBag} (created by \code{\link{InitializeWordBag}}.
#' @return A character vector containing the tidied text.
#' @export
SaveTidiedText <- function(word.bag)
{
    tidied.text <- makeWordBagTextReadable(word.bag$transformed.text)
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
        tokens <- x$final.tokens
        counts <- x$final.counts
    } else if (class(x) == "character") {
        tokenized <- ftaTokenize(x)
        tokens.counts <- countUniqueTokens(tokenized)
        tokens <- tokens.counts$tokens
        counts <- tokens.counts$counts
    }
    if (is.null(alphabetical))
    {
        if (class(x) == "wordBag")
        {
            alphabetical <- x$alphabetical.sort
        } else {
            alphabetical <- FALSE
        }
    }

    tokens <- tokens[counts >= min.frequency]
    counts <- counts[counts >= min.frequency]

    if (alphabetical)
    {
        counts <- counts[order(tokens)]
        tokens <- sort(tokens)
    } else {
        tokens <- tokens[order(counts, decreasing = TRUE)]
        counts <- sort(counts, decreasing = TRUE)
    }

    names(counts) <- makeWordBagTextReadable(tokens)
    return(counts)
}
