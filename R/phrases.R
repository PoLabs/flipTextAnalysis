# Return all sequences of n tokens from each element of the tokenized text
nGramTokenize <- function(tokenized, n)
{
  .getnGrams <- function(x,n)
  {
    x <- x[which(x != " ")] #remove any blank tokens that have crept in
    x <- x[which(x != "")]
    if(length(x) > (n-1))
    {
      y <- vector("character",length = length(x) - n + 1)
      for (j in 1L:length(y))
      {
        y[j] <- paste(x[j:(j+n-1)],collapse = ' ')
      }
      return(y)
    } else {
      return("")
    }
  }

  ngram_tokenized <- vector("list",length=length(tokenized))
  for (j in 1L:length(tokenized))
  {
    ngram_tokenized[[j]] <- .getnGrams(tokenized[[j]],n)
  }
  return(ngram_tokenized)
}


#' \code{GetMostFrequentPhrases}
#'
#' @description Create a data frame containing the most frequent phrases in some text.
#'
#' @param text A character vector containg the text responses.
#' @param num.words A vector of integers specifying the lengths of phrases to be
#'                  considered. For example, \code{c(2,3)} indicates that we want to
#'                  look at phrases with two or three words.
#' @param min.frequency Don't show phrases that appear less often than this value.
#'
#' @return A data frame whose first column contains phrases, and second column contains
#'         counts of those phrases in \code{text}.
#'
#' @examples
#' GetMostFrequentPhrases(ftaFavoriteThings)
#'
#' @export
GetMostFrequentPhrases <- function(text, num.words = c(2,3), min.frequency = 3)
{
  tokenized <- ftaTokenize(text)
  tokens <- vector("character")
  counts <- vector("numeric")
  for (j in 1L:length(num.words))
  {
    ngrams.tokenized <- nGramTokenize(tokenized, num.words[j])
    ngrams.counts <- countUniqueTokens(ngrams.tokenized)
    new.tokens <- ngrams.counts$tokens
    new.counts <- ngrams.counts$counts
    tokens <- c(tokens, new.tokens)
    counts <- c(counts, new.counts)
  }

  tokens <- tokens[order(counts, decreasing = TRUE)]
  counts <- sort(counts, decreasing = TRUE)
  tokens <- tokens[counts > min.frequency]
  counts <- counts[counts > min.frequency]
  phrases <- data.frame("Phrases" = tokens, "Frequencies" = counts)
  class(phrases) <- c("phraseList", "data.frame")
  return(phrases)
}

#' @importFrom flipFormat DataTableWithRItemFormat
#' @export
print.phraseList <- function(x, ...)
{
  dd <- as.data.frame(x)
  my.dt <- DataTableWithRItemFormat(dd)
  print(my.dt)
}

# Convert whitespace within phrases to "+" in order to enable treatment as
# as single unit.
convertPhrasesToTagged <- function(phrases)
{
    split.phrases <- ftaTokenize(phrases)
    tagged.phrases <- sapply(split.phrases, paste, collapse = "+")
    return(tagged.phrases)
}


# This function adds a delimiter to any phrases found in the text.
# In this framework when we want to treat a sequence of words as a
# single unit we join them with "+" so that they don't get split
# by the tokenizer, which uses whitespace. Given a character vector
# of phases, this function iterates through the character vector
# of text and adds the delimiter.
replacePhrasesInText <- function(text, phrases)
{
  tagged.phrases <- convertPhrasesToTagged(phrases)
  for (j in 1L:length(phrases))
  {
    text <- gsub(phrases[j], tagged.phrases[j], text)
  }
  return(text)
}

# Returns a Boolean indicating whether the text has an intevening space.
isPhrase <- function(text)
{
    grepl("[[:print:]][[:space:]][[:print:]]", text)
}
