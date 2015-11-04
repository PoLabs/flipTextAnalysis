# Functions to help interpret input strings from the R Inputs Form in Q

ConvertCommaSeparatedStringToVector = function(string) {
  return(unlist(lapply(strsplit(string, ","), str_trim)))
}

InterpretMergeWordsString = function(string) {
  split.elements = lapply(lapply(strsplit(mystr, ","), str_trim), strsplit, ":")
  return(split.elements)
}
