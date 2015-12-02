


#' \code{FindSpellingErrors}
#'
#' @description Identify spelling errors in a vector of strings by comparing with a dictionary.
#'
#' @param x A vector of words.
#' @param dictionary A vector of words to use as a dictionary. By default this uses an english
#'                   language dictionary, \code{ftaDictionary} which comes with this package.
#'
#' @return A binary vector indicating which of the elements of x are misspelled according to the 
#' input dictionary. When a word is not found, the function also looks for other stems of the
#' word, including \code{word + "s"}, \code{word + "es"}, \code{word + "ed"}, \code{word + "d"},
#' \code{word + "ise"}, and \code{word + "ize"}.
#'
#' @examples
#' FindSpellingErrors(c("hello", "hullo", "hollo", "hollow"))
#'
#' @export
FindSpellingErrors <- function(x, dictionary = ftaDictionary) 
{

  # Binary search, assumes input is sorted
  # Returns the index of val in tab, otherwise -1
  .dictBinSearch <- function(val, tab, L = 1L, H = length(tab))
  {
    while (H >= L) {
      M <- L + (H - L) %/% 2L
      if (tab[M] > val) {
        H <- M - 1L
      }
      else if (tab[M] < val) {
        L <- M + 1L
      }
      else {
        return(M)
      }
    }
    if (L == 1) {
      return(-1)
    }
    if (tab[L-1L] == val) {
      return(L - 1L)
    }
    else {
      return(-1)
    }
  }


  # Function to determine if a word is misspelled
  .spellError <- function(word, dict = ftaDictionary) 
  {
    # Found the word in the dictionary.
    if (.dictBinSearch(word, dict) > -1) 
    {
      return(0)
    } else {

      # Check for various alterations of this word in the dictionary
      sperr <- 1
      word.length <- nchar(word)
      
      if(sperr == 1 & substr(word, word.length, word.length) == "s") 
      {
        reducedword <- substr(word, 1, (word.length-1))
        if(.dictBinSearch(reducedword, dict) > -1) sperr <- 0
      }
      
      if(sperr == 1 & substr(word, word.length-1, word.length) == "es") 
      {
        reducedword <- substr(word, 1, (word.length-2))
        if(.dictBinSearch(reducedword, dict) > -1) sperr <- 0
      }
      
      if(sperr == 1 & substr(word, word.length-1, word.length) == "ed") 
      {
        reducedword <- substr(word, 1, (word.length-2))
        if(.dictBinSearch(reducedword, dict) > -1) sperr <- 0  
      }
      if(sperr == 1 & substr(word, word.length, word.length) == "d") {
        reducedword <- substr(word, 1, (word.length-1))
        if(.dictBinSearch(reducedword, dict) > -1) sperr <- 0
        
      }
      if(sperr == 1 & regexpr("ise", word) > -1) {
        reducedword <- sub("ise", "ize", word)
        if(.dictBinSearch(reducedword, dict) > -1) sperr <- 0
        
      }
    }
    return(sperr)
  }

  if (class(dictionary) != "character") 
  {
    stop(paste("FindSpellingErrors: Expected dictionary to be a character verctor, instead got a: ", class(dictionary)))
  }

  y <- sapply(x, .spellError, dict = dictionary)
  names(y) <- x
  return(y)
}

  
#' \code{GetCorrections}
#'
#' @description Search for corrections for mispelled words within the set of words
#'              identified from some text.
#'
#' @param tokens A character vector containing the set of unique tokens identified
#'               from a set of text.
#' @param counts A numeric vector containing the number of occurrences of each token 
#'               within the text.
#' @param spelling.errors A binary vector indicating which of the tokens have been
#'                        identified as mis-spelled. Obtained, for example, from \code{\link{FindSpellingErrors}}
#' @param do.not.correct A binary vector indicating any tokens which should not be corrected.
#'
#' @details This function is fairly conservative, only returning the most
#'          obvious corrections based on the following heuristics:
#' 1. The correction must begin with the same letter as the misspelling
#' 2. The correction must be within a distance of 1 from the misspelling
#'    ie one insertion, one deletion, one replacement.
#' 3. When there is more than one potential correction for a word, including the original misspelled
#' word, the function chooses the most frequent word as the correction. The original misspelled
#' word is included as brand names will often not appear in the dictionary, but may appear
#' frequently in the text.
#' 
#' @return A character vector containing one correction for each original token.
#'
#' @examples
#' GetCorrections(tokens = c("hello", "hullo", "hollo", "hollow"),
#'                counts = c(10, 5, 4, 5),
#'                spelling.errors = FindSpellingErrors(c("hello", "hullo", "hollo", "hollow")),
#'                do.not.correct = c(0, 0, 0, 0))
#' @export
GetCorrections = function(tokens, counts, spelling.errors, do.not.correct) 
{
  
  # Find the location in x of the first occurrence of each letter of the alphabet.
  # Assumes x is sorted lexicographically.
  .dictionaryLetterIndices <- function(x) 
  {
    letter_index <- list()
    cur_letter <- substr(x[1], 1, 1)
    letter_index[cur_letter] <- 1
    for (j in 2L:length(x)) 
    {
      if(substr(x[j], 1, 1) != cur_letter) 
      {
        cur_letter <- substr(x[j], 1, 1)
        letter_index[cur_letter] <- j
      }
    }
    return(letter_index)
  }

  corrections <- tokens
  correct_words <- tokens[spelling.errors == 0]
  alphabet <- strsplit("abcdefghijklmnopqrstuvwxyz", "")[[1]]
  letter_index <- .dictionaryLetterIndices(correct_words)
  
  # Loop over the words and identify potential corrections for each word which
  # has been identified as an error and is not otherwise excluded.
  for (j in 1L:length(tokens)) 
  {
    if (!do.not.correct[j] & spelling.errors[j]) 
    {
      checkword <- tokens[j]
      firstletter <- substr(checkword, 1, 1)
      if (firstletter %in% names(letter_index)) 
      {
        start <- letter_index[[firstletter]]
        if (which(letter_index == start) < length(letter_index)) 
        {
          end <- letter_index[[which(letter_index == start) + 1]] -1
        } else {
          end <- length(correct_words)
        }

        if (start == end) 
        {
          comparisons <- ""
        } else {
          comparisons <- correct_words[start:end] #check only words that begin with the same letter
        }
        len <- nchar(checkword)
        comparisons <- comparisons[abs(nchar(comparisons) - len) < 2] #check only words whose length is within 1 character
        edist <- cba::sdists(checkword, comparisons, weight = c(1, 1, 0, 1)) # compute edit distances with potential corrections
        potential.corrections <- comparisons[which(edist == 1)] # Identify the list of potential corrections within the limits of the heuristics
        if (length(potential.corrections) > 0) 
        {
          potential.corrections <- c(checkword, potential.corrections)
          potential.corrections <- sort(potential.corrections)
          # Identify the frequencies of the potential corrections
          word.counts <- vector("numeric", length = length(potential.corrections))
          for (k in 1L:length(potential.corrections)) 
          {
            index <- which(tokens == potential.corrections[k])
            word.counts[k] = counts[index]
          } 
          names(word.counts) <- potential.corrections
          max_count <- max(word.counts) # Find the frequency of the most frequent one(s)
          max_corrections <- potential.corrections[which(word.counts == max_count)] # Identify the most frequent correction(s)
          corrections[j] = max_corrections[1] #Pick the first one when there is a tie
        } 
      }
    } 
  }
  return(corrections)
}
