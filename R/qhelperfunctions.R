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
        stopwords <- c(ftaStopList, convertCommaSeparatedStringToVector(extra.stopwords.text))
    } else {
        stopwords <- ftaStopList
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
