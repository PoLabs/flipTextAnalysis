# Clean Text
CleanAndTidyText = function(text, min.frequency = 1, remove.stopwords = TRUE, stoplist = ftaStopList,
  operations = c("spelling", "stemming"), spelling.dictionary = ftaDictionary,
  manual.replacements = NULL) {

	word.bag = InitializeWordBag(text, remove.stopwords = remove.stopwords, stoplist = stoplist, operations = operations, 
		spelling.dictionary = spelling.dictionary, manual.replacements = manual.replacements, min.frequency = min.frequency)
	cleaned.text = word.bag$transformed.text
	class(cleaned.text) = "TidyText"
	return(cleaned.text)
}

print.TidyText = function(text, alphabetical = FALSE) {
  # Print function will show the word frequencies before the cleaned text

  # Tokenize the text
  tokenized = Tokenize(text)

  # Get list of unique tokens and their counts, and sort by descending
  # word frequency
  tokens.counts = CountUniqueTokens(tokenized)
  tokens = tokens.counts$tokens
  counts = tokens.counts$counts

  cat("Frequency of words in the cleaned text:\r\n\r\n")
  cat(printableTokensAndCounts(tokens, counts, alphabetical = alphabetical))
  cat("\r\n\r\n")
  print.default(text)
}


# Generate a string from the tokens and their counts (seprated by : ). If alphabetical is true
# the tokens are printed in alphatical order, otherwise they are printed in order of count
printableTokensAndCounts = function(tokens, counts, alphabetical = FALSE) 
{
  if (length(counts) != length(tokens)) 
  {
    stop("Expected tokens and counts to be the same length")
  }

  tokens = makeWordBagTextReadable(tokens)

  if (alphabetical) 
  {
    counts = counts[order(tokens)]
    tokens = sort(tokens)
  } else {
    tokens = tokens[order(counts, decreasing = TRUE)]
    counts = sort(counts, decreasing = TRUE)
  }


  printable = paste(tokens, counts, collapse = ", ")

  # # Manual wrapping of text
  # n.chars.per.line = 80
  # text.chunks = vector("character")
  # counter = 1
  # reset = TRUE
  # for (j in 1L:length(printable.bits)) 
  # {
  #   if (reset) 
  #   {
  #     current.chunk = printable.bits[j]
  #     reset = FALSE
  #   } else {
  #     current.chunk = paste(current.chunk, printable.bits[j], sep = ", ")
  #   }
  #   if (nchar(current.chunk) > n.chars.per.line) {
  #     reset = TRUE
  #     text.chunks[counter] =  
  #   }
  # }

  return(printable)
}


# Phrases are represented with '+' joining the words,
# but we don't want to print these out for the user.
makeWordBagTextReadable = function(text) 
{
  return(gsub("[+]", " ", text))
}