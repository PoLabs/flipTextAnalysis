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

print.TidyText = function(text) {
  # Print function will show the word frequencies before the cleaned text

  # Tokenize the text
  tokenized = Tokenize(text)

  # Get list of unique tokens and their counts, and sort by descending
  # word frequency
  tokens.counts = CountUniqueTokens(tokenized)
  tokens = tokens.counts$tokens
  counts = tokens.counts$counts
  tokens = tokens[order(counts, decreasing = TRUE)]
  counts = sort(counts, decreasing = TRUE)

  cat("Frequency of words in the cleaned text:\r\n\r\n")
  cat(paste(tokens, rep(":", length(tokens)), counts, collapse = ", "))
  cat("\r\n\r\n")
  print.default(text)
}