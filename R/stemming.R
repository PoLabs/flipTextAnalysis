#' \code{GetStemNames}
#' @description Stem a set of tokens and map each token to the most frequent
#'              token in the set which also has the same stem.
#'
#' @param tokens A character vector containing the tokens to be mapped.
#' @param counts A numeric vector representing the counts of each word.
#' @details On it's own, stemming can often produce text which is not readable
#'          because word stems are often not real words.
#'
#'          This function uses the Snowball stemming algorithm from the package
#'          \code{SnowballC} and some additional heuristics to map each token
#'          to the token in the set which has the same stem and the highest count.
#'          For example, if the tokens \code{search} and \code{searched} have
#'          counts of 4 and 5 in the text, then both will be mapped to \code{searched}
#'          as it appears more frequently.
#'
#' @return A character vector with the same length as \code{tokens} that contains the
#'         new mapped tokens.
#' @export
GetStemNames <- function(tokens, counts) 
{


  # Return the words in x which occur most frequently
  # according to counts. Multiple words are returned if
  # they have the same frequency.
  .getMostFrequentWords = function(x, words, counts) {
    cc = counts[words%in%x]
    ww = words[words%in%x]
    max_count = max(cc)
    max_words = ww[which(cc==max_count)]
    return(max_words)
  }

  # Get stems for the input tokens using the Snowball stemmer
  .getStems <- function(x) 
  {
    stems = vector("character", length = length(x))
    for (j in 1L:length(x)) 
    {
      stems[j] = SnowballC::wordStem(x[j])
    }
    return(stems)
  }
  
  if (length(counts) != length(tokens)) 
  {
    stop("Expected tokens and counts to be the same length")
  }

  # Obtain the stemmed version of each token
  cur.stems <- .getStems(tokens)
  unique.stems <- unique(cur.stems)

  # For each unique stem, collect all of the tokens which map to that stem
  stem.sets <- vector("list", length = length(unique.stems))
  for (j in 1L:length(stem.sets)) 
  {
    stem.sets[[j]] = tokens[which(cur.stems == unique.stems[j])]
  }
  
  # For each unique stem, determine which of the tokens it matches is most
  # frequent. If multiple words are equal first in frequency, pick the
  # first one in the list.
  stem.identifiers <- vector("character", length = length(unique.stems))
  for (j in 1L:length(unique.stems)) 
  {
    max.words <- .getMostFrequentWords(stem.sets[[j]], tokens, counts)
    stem.identifiers[j] <- max.words[1]
  }

  stem.names <- vector("character",length = length(tokens))
  for (j in 1L:length(tokens)) 
  {
    stem.names[j] <- stem.identifiers[which(unique.stems == cur.stems[j])]
  }
  return(stem.names)
}
