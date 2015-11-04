# Go

#' Create a bag-of-words
#'
#' Create a bag-of-words object from a vector of strings.
#'
#' @param text A character vector containing the text to be analyzed.
#' @param remove.stopwords A boolean value specifying whether or not to identify stopwords and remove them from subsequent analyses.
#' @param stoplist A character vector containg the stopwords. The default value is this package's built-in stopwords list, ftaStopList.
#' @param correct.spelling A boolean value specifying whether or not to identify spelling mistakes and to generate corrections.
#' @param spelling.dictionary A character vector containing the dictionary to use to check each word for mis-spellings. The default value is this package's built-in english dictionary, ftaDictionary.
#' @param do.stemming A boolean value specifying whether or not to stem the words and replace each stem with the most commonly-occuring word that matches that stem.
#' @param manual.replacements A matrix of characters with two columms. The first column specifies the words to replace, and the second column specifies the corresponding replacements.
#' @return A list containing the word bag details
#'
#' @examples
#' wb = InitializeWordBag(nasaTweetText)
InitializeWordBag = function(text, remove.stopwords = TRUE, stoplist = ftaStopList,
  correct.spelling = TRUE, spelling.dictionary = ftaDictionary,
  do.stemming = TRUE, manual.replacements = NULL) {

  word.bag = list()
  tokenized = Tokenize(text)

  # Get list of unique tokens and their counts
  tokens.counts = CountUniqueTokens(tokenized)
  tokens = tokens.counts$tokens
  counts = tokens.counts$counts



  # If manual replacements have been specified, these words need to
  # be added to the tokens
  if (!is.null(manual.replacements)) {

    # The first column of the replacements can't have duplicates.
    # Which of the duplicates should be mapped?
    duplicates = manual.replacements[duplicated(manual.replacements[, 1]), 1]
    if (length(duplicates) > 0) {
      stop(paste("manual.replacements contains duplicates in the first column, for example: ", duplicates[1], sep = ""))
    }

    # Add entries for any new tokens to the existing vector of tokens
    # and initialize the count to zero
    extra.tokens = as.vector(manual.replacements)
    extra.tokens = unique(extra.tokens)
    
    for (j in 1L:length(extra.tokens)) {
      if (! extra.tokens[j] %in% tokens) {
        current.length = length(tokens)
        tokens[current.length + 1] = extra.tokens[j]
        counts[current.length + 1] = 0
      }
    }
  }

  # These keep track of the tokens as we move through the different processes
  current.tokens = tokens
  current.counts = counts

  word.bag$original.text = text
  word.bag$tokens = tokens
  word.bag$counts = counts
  word.bag$tokenized = tokenized
  word.bag$stopwords = rep(0, length(tokens))
  word.bag$spelling.corrected = FALSE
  word.bag$stemmed = FALSE
  word.bag$manual.replacements = manual.replacements
  class(word.bag) = "wordBag"

  if (remove.stopwords) {
    word.bag$stopwords = FindStopWords(current.tokens, stoplist)
  }


  if (correct.spelling) {
    spelling.errors = FindSpellingErrors(current.tokens, spelling.dictionary)
    corrected.tokens = GetCorrections(current.tokens, current.counts, spelling.errors, do.not.correct = word.bag$stopwords)
    word.bag$corrected.tokens = corrected.tokens #remove
    word.bag$spelling.corrected = TRUE
    corrected.counts = GetUpdatedCounts(current.tokens, current.counts, corrected.tokens)
    word.bag$corrected.counts = corrected.counts #remove
    current.tokens = corrected.tokens
    current.counts = corrected.counts
  }

  # Conduct manual replacements
  if (!is.null(manual.replacements)) {
    # Do replacements
    new.tokens = current.tokens
    for (j in 1L:length(tokens)) {
      index = which(manual.replacements[, 1] == new.tokens[j])
      if (length(index) > 0) {
        new.tokens[j] = manual.replacements[index, 2]
      }
    }

    # Update counts
    new.counts = GetUpdatedCounts(current.tokens, current.counts, new.tokens)

    # Update our current tokens and counts
    current.tokens = new.tokens
    current.counts = new.counts
    #print(cbind(tokens, current.tokens))
  }

  if (do.stemming) {
    stemmed.tokens = GetStemNames(current.tokens, current.counts)
    word.bag$stemmed = TRUE
    word.bag$stemmed.tokens = stemmed.tokens #remove
    stemmed.counts = GetUpdatedCounts(current.tokens, current.counts, stemmed.tokens)
    word.bag$stemmed.counts = stemmed.counts #remove
    current.tokens = stemmed.tokens
    current.counts = stemmed.counts
  }

  # Transform the original text
  if (do.stemming || correct.spelling || remove.stopwords || !is.null(manual.replacements) ) {

    if (remove.stopwords) {
      transformed.tokenized = lapply(tokenized, setdiff, y = tokens[word.bag$stopwords == 1])
    }

    transformed.tokenized = MapTokenizedText(transformed.tokenized, before = tokens, after = current.tokens)

    word.bag$final.tokens = current.tokens
    word.bag$final.counts = current.counts
    word.bag$transformed.tokenized = transformed.tokenized
    word.bag$transformed.text = sapply(transformed.tokenized, paste, collapse = " ")

  } else {
    word.bag$final.tokens = NULL
    word.bag$final.counts = NULL
    word.bag$transformed.tokenized = NULL
    word.bag$transformed.text = NULL
  }

  return(word.bag)
}
