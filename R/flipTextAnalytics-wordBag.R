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
#' @param phrases A character vector containing strings that should be treated as single units during processing.
#' @param min.frequency An integer specifiying the smallest frequency of word to keep in the transformed text.
#' @return A list containing the word bag details
#'
#' @examples
#' wb = InitializeWordBag(nasaTweetText)
InitializeWordBag = function(text, remove.stopwords = TRUE, stoplist = ftaStopList,
  operations = c("spelling", "stemming"), spelling.dictionary = ftaDictionary,
  manual.replacements = NULL, phrases = NULL, min.frequency = 1, alphabetical.sort = TRUE) 
{

  # Check that the options supplied make sense
  checkWordBagOperations(operations = operations, 
                         remove.stopwords = remove.stopwords, 
                         stoplist = stoplist, 
                         spelling.dictionary = spelling.dictionary, 
                         manual.replacements = manual.replacements)

  word.bag = list()
  original.text = text

  # Convert all the text to lowercase first
  # Makes all other steps easier
  text = tolower(text)
  

  # Catch any phrases specified by the user now
  # so that they get treated as a unit.
  # Phrases are two or more words, where each
  # word is joined by a +. Never print the +
  if (!is.null(phrases)) 
  {
    phrases = tolower(phrases)
    text = replacePhrasesInText(text, phrases)
  }



  # Tokenize the text
  tokenized = Tokenize(text)

  # Get list of unique tokens and their counts
  tokens.counts = CountUniqueTokens(tokenized)
  tokens = tokens.counts$tokens
  counts = tokens.counts$counts

  # If manual replacements have been specified, these words need to
  # be added to the tokens
  if (!is.null(manual.replacements)) {
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

  word.bag$original.text = original.text
  word.bag$tokens = tokens
  word.bag$counts = counts
  word.bag$tokenized = tokenized
  word.bag$manual.replacements = manual.replacements
  word.bag$alphabetical.sort = alphabetical.sort
  class(word.bag) = "wordBag"

  if (remove.stopwords) {
    word.bag$stopwords = FindStopWords(current.tokens, stoplist)
  }

  for (j in 1L:length(operations)) {
    op = operations[j]
    if (op == "spelling") {
      spelling.errors = FindSpellingErrors(current.tokens, spelling.dictionary)
      corrected.tokens = GetCorrections(current.tokens, current.counts, spelling.errors, do.not.correct = word.bag$stopwords)
      word.bag$corrected.tokens = corrected.tokens #remove
      word.bag$spelling.corrected = TRUE
      corrected.counts = GetUpdatedCounts(current.tokens, current.counts, corrected.tokens)
      word.bag$corrected.counts = corrected.counts #remove
      current.tokens = corrected.tokens
      current.counts = corrected.counts
    } else if (op == "replacement") {
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
    } else if (op == "stemming") {
      stemmed.tokens = GetStemNames(current.tokens, current.counts)
      word.bag$stemmed = TRUE
      word.bag$stemmed.tokens = stemmed.tokens #remove
      stemmed.counts = GetUpdatedCounts(current.tokens, current.counts, stemmed.tokens)
      word.bag$stemmed.counts = stemmed.counts #remove
      current.tokens = stemmed.tokens
      current.counts = stemmed.counts
    }
  }

  # Transform the original text
  if (remove.stopwords || length(operations) > 0 || min.frequency > 1 || !is.null(phrases)) {

    replace.tokens = current.tokens
    if (remove.stopwords) {
      # transformed.tokenized = lapply(tokenized, setdiff, y = tokens[word.bag$stopwords == 1])
      replace.tokens[which(word.bag$stopwords == 1)] = ""
    }

    # Remove words which are less frequent that the minimum specified
    if (min.frequency > 1) {
      replace.tokens[which(current.counts < min.frequency)] = ""
    }
    transformed.tokenized = MapTokenizedText(tokenized, before = tokens, after = replace.tokens)
    transformed.text = sapply(transformed.tokenized, paste, collapse = " ")

    # Do a final phrase replacement and get final tokens
    if (!is.null(phrases)) {
      transformed.text = replacePhrasesInText(transformed.text, phrases)
    }

    transformed.tokenized = Tokenize(transformed.text)

    tokens.counts = CountUniqueTokens(transformed.tokenized)
    current.tokens = tokens.counts$tokens
    current.counts = tokens.counts$counts

    word.bag$transformed.tokenized = transformed.tokenized
    word.bag$transformed.text = transformed.text

  } else {
    word.bag$transformed.tokenized = text
    word.bag$transformed.text = tokenized
  }

  word.bag$final.tokens = current.tokens
  word.bag$final.counts = current.counts

  return(word.bag)
}


checkWordBagOperations = function(operations, remove.stopwords, stoplist, spelling.dictionary, manual.replacements) {
  
  valid.operations = c("spelling", "replacement", "stemming", "")
  invalid.operations = operations[which(! operations %in% valid.operations)]
  if (length(invalid.operations) > 0) {
    stop(paste(invalid.operations[1], " is not a valid operation. Valid operations are: ", valid.operations, sep = ""))
  }

  if ("replacement" %in% operations) {
    if (is.null(manual.replacements)) {
      stop("A manual replacement step has been included in the wordbag operations, but no replacements have been specified.")
    }

    # Check dimensions and properties of replacements matrix
    if (class(manual.replacements) != "matrix" 
      || class(manual.replacements[1, ]) != "character" 
      || ncol(manual.replacements) != 2) {
      stop("Manual replacements should be specified as a two-column matrix with entries that are characters (words).")
    }

    # The first column of the replacements can't have duplicates.
    # Which of the duplicates should be mapped?
    duplicates = manual.replacements[duplicated(manual.replacements[, 1]), 1]
    if (length(duplicates) > 0) {
      stop(paste("manual.replacements contains duplicates in the first column, for example: ", duplicates[1], sep = ""))
    }
  }

  if (remove.stopwords) {
    if (is.null(stoplist)) {
      stop("Stopword removal has been used, but no list of stopwords has been provided.")
    }
    if (class(stoplist) != "character") {
      stop("'stoplist' should be a character vector.")
    }
  }

  if ("spelling" %in% operations) {
    if (is.null(spelling.dictionary)) {
      stop("A spelling correction step has been included in the opearations, but no dictionary has been provided.")
    }
    if (class(spelling.dictionary) != "character") {
      stop("'spelling.dictionary' should be a vector of characters.")
    }
  }

  if (!is.null(manual.replacements) && ! "replacement" %in% operations) {
    stop("Replacements have been specified, but the list of operations does not contain a 'replacement' step.")
  }

  return(TRUE)
}



print.wordBag = function(x) {
  cat("Word Frequencies:\r\n\r\n")
  cat(printableTokensAndCounts(x$final.tokens, x$final.counts, alphabetical = wb$alphabetical.sort))
  cat("")
  cat("\r\n\r\nText Cleaning:\r\n\r\n")
  output.text = cbind(x$original.text, makeWordBagTextReadable(x$transformed.text))
  colnames(output.text) = c("Original text", "Tidied Text")
  rownames(output.text) = NULL
  output.text = head(output.text, 10)
  print(output.text)
  cat("...")
}