# Functions to help interpret input strings from the R Inputs Form in Q

ConvertCommaSeparatedStringToVector = function(string) {
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
InterpretMergeWordsString = function(string) {
  split.elements = sapply(strsplit(string, "[:,]"), stringr::str_trim)[, 1]
  replacement.matrix = matrix(split.elements, ncol = 2, byrow = TRUE)
  return(replacement.matrix)
}
