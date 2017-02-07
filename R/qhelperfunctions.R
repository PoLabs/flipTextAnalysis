# Functions to help interpret input strings from the R Inputs Form in Displayr

# Split up a user-input string by commas
convertCommaSeparatedStringToVector <- function(string)
{
    string <- stringr::str_trim(string)
    return(unlist(lapply(strsplit(string, ","), stringr::str_trim)))
}

# Helper function to interpret replacements specified by the user in Q
# and get them into the right shape for the wordbag creation.
# Accepts a string of comma-separated pairs of words (with the pairs
# separated by colons).
# Converts a string of the form: "A:B, C:D, E:F" to a matrix
# A B
# C D
# E F
interpretMergeWordsString <- function(string)
{
  split.elements <- sapply(strsplit(string, "[:,]"), stringr::str_trim)[, 1]
  replacement.matrix <- matrix(split.elements, ncol = 2, byrow = TRUE)
  return(replacement.matrix)
}

# Generate a simple sequence of word bag operations based on whether or not
# spelling, stemming, and replacements are specified.
generateOperations <- function(do.spell, do.stem, replacement.matrix)
{
	counter <- 1;
	operations <- vector("character", 1)
	if (do.spell)
	{
	    operations[counter] <- "spelling"
	    counter = counter + 1
	}
	if (!is.null(replacement.matrix)) {
	    operations[counter] <- "replacement"
	    counter <- counter + 1
	}
	if (do.stem) {
	    operations[counter] <- "stemming"
	}
	return(operations)
}

# Convert the set of user inputs into options for the word bag creation.
getTextAnalysisOptions <- function(phrases, extra.stopwords.text, replacements.text, do.stem, do.spell)
{
    GetTextAnalysisOptions(phrases, extra.stopwords.text, replacements.text, do.stem, do.spell)
}

#' \code{GetTextAnalysisOptions}
#' @description This function takes a set of options specified by the user, combines them with the
#' standard options for text-cleaning, and generates a simple sequence of operations for the creation
#' of a word bag. Word bag creation can take an arbitrary sequence, and this function interprets
#' the user's inputs and converts it into a sensible, simple sequence.
#' @param phrases A string which contains pairs of words, separated by commas, to be treated as phrases
#' rather than as separate words. For example: "hello world, very happy" would instruct the options
#' to treat appearances of "hello world" and "very happy" as single units, rather than as the individual
#' words "hello", "world", "very", and "happy".
#' @param extra.stopwords.text A string containing additional words to be removed from the analysis. The
#' words should be separated by commas.
#' @param replacements.text A string containing instructions on how to replace particular words. The
#' string should be in the form \code{"<word to replace 1>:<replacement word 1>, <word to replace 2>:<replacement word 2>"}.
#' So to replace "good" with "great", and "like" with "love", the string should be \code{"good:great,like:love"}.
#' @param do.stem A boolean value indicating whether words should be stemmed.
#' @param do.spell A boolean value indicating whether words should be spell-checked and corrected.
#' @export
GetTextAnalysisOptions <- function(phrases = "", extra.stopwords.text = "", replacements.text = "", do.stem = FALSE, do.spell = FALSE)
{
    replacements.text <- tolower(replacements.text)
    if (nchar(phrases) > 0 && regexpr(",", phrases) > 1)
    {
        phrases <- convertCommaSeparatedStringToVector(phrases)
        if (length(phrases) == 0)
        {
            phrases <- NULL
        }
    }

    if (nchar(extra.stopwords.text) > 0)
    {
        extra.stopwords.text <- tolower(extra.stopwords.text)
        stopwords <- c(get("ftaStopList"), convertCommaSeparatedStringToVector(extra.stopwords.text))
    } else {
        stopwords <- get("ftaStopList")
    }

    if (nchar(replacements.text) > 2 && regexpr(":", replacements.text) > 1)
    {
        replacement.matrix <- interpretMergeWordsString(replacements.text)
    } else {
        replacement.matrix <- NULL
    }
    operations <- generateOperations(do.spell, do.stem, replacement.matrix)
    return(list(phrases = phrases,
                stopwords = stopwords,
                replacement.matrix = replacement.matrix,
                operations = operations
    ))
}
