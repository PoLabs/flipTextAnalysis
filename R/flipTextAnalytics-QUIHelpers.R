# Functions to help interpret input strings from the R Inputs Form in Q

convertCommaSeparatedStringToVector = function(string) {
  return(unlist(lapply(strsplit(string, ","), str_trim)))
}

interpretMergeWordsString = function(string) {
  split.elements = lapply(lapply(strsplit(mystr, ","), str_trim), strsplit, ":")
  return(split.elements)
}
