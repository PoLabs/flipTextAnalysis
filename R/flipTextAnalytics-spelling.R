# Binary search, assumes input is sorted
# Returns the index of val in tab, otherwise -1
dictBinSearch <- function(val, tab, L=1L, H=length(tab))
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



# Determine if a word is misspelled
spellerror <- function(word, dict = ftaDictionary) {
  if (length(grep("[:alpha:]", word)) == 0) {
    return(0)
  }
  if (dictBinSearch(word, dict) > -1) {
    return(0)
  }
  else {
    sperr <- 1
    word.length = nchar(word)
    if(sperr == 1 & substr(word, word.length, word.length) == "s") {
      reducedword <- substr(word, 1, (word.length-1))
      if(dictBinSearch(reducedword, dict) > -1) sperr <- 0
    }
    if(sperr == 1 & substr(word, word.length-1, word.length) == "es") {
      reducedword <- substr(word, 1, (word.length-2))
      if(dictBinSearch(reducedword, dict) > -1) sperr <- 0
    }
    if(sperr == 1 & substr(word, word.length-1, word.length) == "ed") {
      reducedword <- substr(word, 1, (word.length-2))
      if(dictBinSearch(reducedword, dict) > -1) sperr <- 0
    }
    if(sperr == 1 & substr(word, word.length, word.length) == "d") {
      reducedword <- substr(word, 1, (word.length-1))
      if(dictBinSearch(reducedword, dict) > -1) sperr <- 0
    }
    if(sperr == 1 & regexpr("ise", word) > -1) {
      reducedword <- sub("ise", "ize", word)
      if(dictBinSearch(reducedword, dict) > -1) sperr <- 0
    }
  }
  return(sperr)
}


# Return a binary vector indicating which of the elements of x are
# misspelled according to the input dictionary and the rules in
# the function spellerror
findSpellingErrors = function(x, dictionary = ftaDictionary) {
  if (class(dictionary) != "character") {
    stop(paste("findSpellingErrors: Expected dictionary to be a character verctor, instead got a: ", class(dictionary)))
  }
  y = sapply(x, spellerror, dict=dictionary)
  return(y)
}

# Find the location in x of the first occurrence of each letter of the alphabet.
# This is used to search for spelling corrections.
# Assumes x is sorted lexicographically.

dictionaryLetterIndices <- function(x) {
  letter_index <- list()
  cur_letter <- substr(x[1],1,1)
  letter_index[cur_letter] <- 1
  for (j in 2L:length(x)) {
    if(substr(x[j],1,1) != cur_letter) {
      cur_letter <- substr(x[j],1,1)
      letter_index[cur_letter] <- j
    }
  }
  return(letter_index)
}

# Function to idenfify potential corrections for misspelled words.
# This function is fairly conservative, only returning the most
# obvious corrections based on the following heuristics:
# 1. The correction must begin with the same letter as the misspelling
# 2. The correction must be within a distance of 1 from the misspelling
#    ie one insertion, one deletion, one replacement.
#
# Given a list of potential corrections, including the original misspelled
# word, the function chooses the most frequent word as the correction.

getCorrections = function(wb) {
  library(cba)
  corrections = vector("character",length = length(wb$tokens))
  correct_words = wb$tokens[wb$spelling_errors == 0]
  alphabet = strsplit("abcdefghijklmnopqrstuvwxyz","")[[1]]
  letter_index = dictionaryLetterIndices(wb$tokens[wb$spelling_errors == 0])
  for (j in 1L:length(wb$tokens)) {
    if (!wb$stopwords[j] & wb$spelling_errors[j]) {
      checkword = wb$tokens[j]
      firstletter = substr(checkword,1,1)
      if (firstletter%in%names(letter_index)) {
        start = letter_index[[firstletter]]
        if (which(letter_index==start) < length(letter_index)) {
          end = letter_index[[which(letter_index==start)+1]] -1
        } else {end = length(correct_words)}
        if (start == end) comparisons = ""
        else comparisons = correct_words[start:end] #check only words that begin with the same letter
        len = nchar(checkword)
        comparisons = comparisons[abs(nchar(comparisons) - len) < 2] #check only words whose length is within 1 character
        edist = sdists(checkword,comparisons,weight=c(1,1,0,1)) # compute edit distances with potential corrections
        potential_corrections = comparisons[which(edist == 1)] # Identify the list of potential corrections within the limits of the heuristics
        if (length(potential_corrections) >0) {
          potential_corrections = c(checkword, potential_corrections)
          wc = wb$counts[which(wb$tokens%in%potential_corrections)] # Identify the frequencies of the potential corrections
          max_count = max(wc) # Find the frequency of the most frequent one(s)
          max_corrections = potential_corrections[which(wc == max_count)] # Identify the most frequent correction(s)
          corrections[j] = max_corrections[1]
        } else corrections[j] = wb$tokens[j] # No corrections found within limits of heuristics
      } else corrections[j] = wb$tokens[j]
    } else corrections[j] = wb$tokens[j]
  }
  return(corrections)
}

getCorrectedCounts <- function(wb) {
  if (wb$spelling_corrected) {
    corrected_counts <- wb$counts
    for (j in 1L:length(wb$tokens)) {
      if (wb$tokens[j] != wb$map[j,1]) {
        corrected_counts[which(wb$tokens == wb$map[j,1])] <- corrected_counts[j] + corrected_counts[which(wb$tokens == wb$map[j,1])]
        corrected_counts[j] <- 0
      }
    }
    return(corrected_counts)
  } else return(NULL)
}

