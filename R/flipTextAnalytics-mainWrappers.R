TermMatrixFromWordBag = function(word.bag, min.frequency = 5) {
	return(termMatrixFromText(word.bag$transformed.text))
}


# Wrapper function for package tm's Term Matrix Construction
# Returns a matrix with documents in rows, and terms in columns.
termMatrixFromText = function(text, min.frequency) {
  source = tm::VectorSource(text)
  corpus = tm::VCorpus(source)
  my.tdm = tm::DocumentTermMatrix(corpus)
  my.tdm = tm::weightBin(my.tdm)
  # Get words which appear with certain frequency
  # and generate term matrix based on those words
  my.dictionary = tm::findFreqTerms(my.tdm, lowfreq = min.frequency)
  my.tdm = tm::DocumentTermMatrix(corpus, list(dictionary = my.dictionary))
  my.tdm = tm::weightBin(my.tdm)
  return(tm::inspect(my.tdm))
}

AsTermMatrix = function(x, min.frequency = 5, remove.stopwords = TRUE, stoplist = ftaStopList,
  operations = c("spelling", "stemming"), spelling.dictionary = ftaDictionary,
  manual.replacements = NULL) {
  if (class(x) == "wordBag") {
    stop("Input argument is a wordBag object. Use function TermMatrixFromWordBag() instead.")
  } else if (class(x) == "character") {
    word.bag = InitializeWordBag(x, remove.stopwords = remove.stopwords, stoplist = stoplist, operations = operations, 
                                spelling.dictionary = spelling.dictionary, manual.replacements = manual.replacements)
    tdm = termMatrixFromText(word.bag$transformed.text, min.frequency = min.frequency)
  } else if (class(x) == "TidyText") {
    tdm = termMatrixFromText(x, min.frequency = min.frequency)
  }
	return(tdm)
}

SentimentScoresFromWordBag = function(word.bag, pos.words = ftaPositiveWords, neg.words = ftaNegativeWords) {
	tagged.text = TagSentiment(word.bag$tokens, pos.words, neg.words)
	sentiment.scores = lapply(word.bag$transformed.text, ScoreSentimentForString, tokens = word.bag$tokens, sentiment.tags = tagged.text)
	sentiment.matrix = matrix(unlist(sentiment.scores), nrow = length(sentiment.scores), byrow = TRUE)
	sentiment.matrix = cbind(sentiment.matrix, sentiment.matrix[, 1] - sentiment.matrix[, 2])
	colnames(sentiment.matrix) = c("Positive words", "Negative words", "Sentiment score")
	rownames(sentiment.matrix) = word.bag$original.text
	return(sentiment.matrix)
}


AsSentimentMatrix = function(text, remove.stopwords = TRUE, stoplist = ftaStopList,
  operations = c("spelling", "stemming"), spelling.dictionary = ftaDictionary,
  manual.replacements = NULL, pos.words = ftaPositiveWords, 
  neg.words = ftaNegativeWords) {
	word.bag = InitializeWordBag(text, remove.stopwords = remove.stopwords, stoplist = stoplist, operations = operations, 
		spelling.dictionary = spelling.dictionary, manual.replacements = manual.replacements)
	sentiment.matrix = SentimentScoresFromWordBag(word.bag, pos.words = pos.words, neg.words = neg.words)
}

MostFrequentWords = function(text, min.frequency = 2, remove.stopwords = TRUE, stoplist = ftaStopList,
  operations = c("spelling", "stemming"), spelling.dictionary = ftaDictionary,
  manual.replacements = NULL) {
  	word.bag = InitializeWordBag(text, remove.stopwords = remove.stopwords, stoplist = stoplist, operations = operations, 
		spelling.dictionary = spelling.dictionary, manual.replacements = manual.replacements)
  	tokens = word.bag$final.tokens
  	counts = word.bag$final.counts
  	if (remove.stopwords) {
  		tokens = tokens[word.bag$stopwords == 0]
  		counts = counts[word.bag$stopwords == 0]
  	}
  	tokens = tokens[order(counts, decreasing = TRUE)]
  	counts = sort(counts, decreasing = TRUE)
  	tokens = tokens[counts >= min.frequency]
  	counts = counts[counts >= min.frequency]
  	return(data.frame(tokens, counts))
}