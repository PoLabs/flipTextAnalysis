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
#' @param manual.replacements A list of character vectors (pairs of words)
#' @return A list containing the word bag details
#'
#' @examples
#' wb = InitializeWordBag(nasaTweetText)
InitializeWordBag = function(text, remove.stopwords = TRUE, stoplist = ftaStopList,
  correct.spelling = TRUE, spelling.dictionary = ftaDictionary,
  do.stemming = TRUE, manual.replacements = NULL) {

  word.bag = list()
  tokenized = tokenize(text)
  if (!is.null(manual.replacements)) {
    tokenized$mantokens = unlist(manual.replacements)
  }
  tokens = vector("character", length = 1000)
  counts = vector("integer", length = 1000)
  word_counter = 1
  # Collect and count unique tokens
  for (j in 1L:length(tokenized)) {
    curtokes = tokenized[[j]]
    if (length(curtokes) > 0) {
      for (k in 1L:length(curtokes)) {
        cur.word = curtokes[k]
        if (nchar(cur.word) > 0 & !is.na(cur.word)) {
          ind = which(tokens == cur.word)
          if (length(ind) == 0) {
            tokens[word_counter] = cur.word
            counts[word_counter] = 1
            word_counter = word_counter + 1
            if (word_counter >= length(tokens)) {
              tokens = c(tokens, vector("character", length = 1000))
              counts = c(counts, vector("integer", length = 1000))
            }
          } else counts[ind] = counts[ind] + 1
        }
      }
    }
  }



  tokens = tokens[1:(word_counter - 1)]
  counts = counts[1:(word_counter - 1)]
  counts = counts[order(tokens)]
  tokens = sort(tokens)
  word.bag$original.text = text
  word.bag$tokens = tokens
  word.bag$counts = counts
  word.bag$tokenized = tokenized
  word.bag$word.lengths = sapply(tokens, nchar)
  word.bag$map = matrix("", nrow = length(tokens), ncol = 2)
  word.bag$stopwords = rep(0, length(tokens))
  word.bag$corrected.counts = vector("integer", length = length(tokens))
  word.bag$stemmed.counts = vector("integer", length = length(tokens))
  word.bag$spelling.corrected = FALSE
  word.bag$stemmed = FALSE
  class(word.bag) = "wordBag"

  if (!is.null(manual.replacements)) {

  }

  if (remove.stopwords) {
    word.bag$stopwords = findStopWords(tokens, stoplist)
  }

  if (correct.spelling) {
    word.bag$spelling.errors = findSpellingErrors(tokens, spelling.dictionary)
    word.bag$corrected.tokens = getCorrections(tokens, counts, word.bag$spelling.errors, do.not.correct = word.bag$stopwords)
    word.bag$spelling.corrected = TRUE
    word.bag$corrected.counts = getUpdatedCounts(tokens, counts, word.bag$corrected.tokens)
  }

  if (do.stemming) {
    if (correct.spelling) {
      word.bag$stemmed.tokens = getStemNames(word.bag$corrected.tokens, word.bag$corrected.counts)
    } else {
      word.bag$stemmed.tokens = getStemNames(tokens, counts)
    }
    word.bag$stemmed = TRUE
    word.bag$stemmed.counts = getUpdatedCounts(tokens, counts, word.bag$stemmed.tokens)
  }

  # Transform the original text
  if (do.stemming || correct.spelling || remove.stopwords) {
    final.tokens = NULL
    if (do.stemming) {
      final.tokens = word.bag$stemmed.tokens
    } else if (correct.spelling) {
      final.tokens = word.bag$corrected.tokens
    }

    if (remove.stopwords) {
      transformed.tokenized = lapply(tokenized, setdiff, y = tokens[word.bag$stopwords == 1])
    }

    if (!is.null(final.tokens)) {
      transformed.tokenized = mapTokenizedText(transformed.tokenized, before = tokens, after = final.tokens)
    }

    word.bag$transformed.tokenized = transformed.tokenized

  } else {
    word.bag$transformed.tokenized = NULL
  }

  return(word.bag)
}
