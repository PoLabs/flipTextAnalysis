#' \code{TagSentiment}
#'
#' @description Identify words as positive, negative, or neutral.
#'
#' @param tokens A character vector containing the tokens to tag.
#' @param pos.words A character vector containing the dictionary of positive words.
#' @param neg.words A character vector containing the dictionary of negative words.
#'
#' @return A character vector with the same length as \code{tokens} which has entries
#'          \code{[pos]} for postive words, \code{[neg]} for negative words, and
#'          \code{[neu]} for neutral words (i.e. not found in either dictionary.)
#'
#' @details This function does not handle negation, but only looks for each token with
#' the two dictionaries. Negation can be handled later by looking at the context of each
#' token in the original text, as done by \code{\link{ScoreSentimentForString}}.
#'
#' @examples
#' TagSentiment(c("great","happy","the", "of", "bad", "terrible"))
#' @export
TagSentiment = function(tokens, pos.words = ftaPositiveWords, neg.words = ftaNegativeWords) {
  tagger = function(token) {
    if (token %in% pos.words) {
      return("[pos]")
    } else if (token %in% neg.words) {
      return("[neg]")
    } else {
      return("[neu]")
    }
  }
  return(sapply(tokens, tagger))
}

#' \code{ScoreSentimentForString}
#'
#' @description Generate scores of positive and negative sentiment for a string.
#'
#' @param string The string to evaulate.
#' @param tokens A vector of strings that have been identified in the text from which
#'               \code{string} has been drawn.
#' @param sentiment.tags A vector of sentiment tags for the \code{tokens}, obtained
#'                       by \code{\link{TagSentiment}}.
#'
#' @return A vector of two integers, where the first indicates the positive sentiment
#'          score, and the second contains the negative sentiment score.
#'
#' @details This function attempts to handle some negation of sentiment by looking at the
#'          words preceding a sentiment word and check for things like \code{"not"},
#'          \code{"wasnt"}, etc.
#'
#' @examples
#' my.tokens <- c("im", "not", "very", "happy")
#' my.tags <- TagSentiment(my.tokens)
#' ScoreSentimentForString("im not very happy",
#'                          tokens = my.tokens,
#'                          sentiment.tags = my.tags)
#'
#' @export
ScoreSentimentForString = function(string, tokens, sentiment.tags)
{
    negation.unigrams <- c("no", "not", "wasnt", "werent", "wouldnt", "couldnt", "didnt", "never", "nobody", "nothing", "dont")
    negation.bigrams <- c("not very")

    current.tokens <- unlist(ftaTokenize(string))
    current.tokens <- stringr::str_trim(current.tokens)
    current.tokens <- current.tokens[which(nchar(current.tokens) > 0)]
    # nothing left after tokenization
    if (length(current.tokens) == 0)
    {
        return(c(0,0))
    }

    current.tags <- sapply(current.tokens, mapToken, source.tokens = tokens, target.tokens = sentiment.tags)
    positive.score <- 0
    negative.score <- 0

    # Loop through the tokens and tags.
    # Count positive and negative tokens whilst
    # looking at preceding terms for negations
    for (j in 1L:length(current.tokens))
    {
        if (current.tags[j] == "[pos]" || current.tags[j] == "[neg]")
        {
            is.negated = FALSE
            # Look for preceding unigrams
            if (j > 1)
            {
                if (current.tokens[j-1] %in% negation.unigrams)
                {
                    is.negated = TRUE
                }
            }

            # If not already negated, look for preceding bigrams
            if (!is.negated && j > 2)
            {
                if (paste(current.tokens[j-2], current.tokens[j-1]) %in% negation.bigrams)
                {
                    is.negated = TRUE
                }
            }

            # Increment the score appropriately
            if ( (!is.negated && current.tags[j] == "[pos]") || (is.negated && current.tags[j] == "[neg]") )
            {
                positive.score <- positive.score + 1
            } else {
                negative.score <- negative.score + 1
            }
        }
    }
    scores <- c(positive.score, negative.score)
    names(scores) <- c("positive.score", "negative.score")
    return(c(positive.score, negative.score))
}
