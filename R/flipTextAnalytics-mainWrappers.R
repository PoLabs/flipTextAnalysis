TermMatrixFromWordBag = function(word.bag) {
	source = tm::VectorSource(word.bag$transformed.text)
    corpus = tm::VCorpus(source)
    my.tdm = tm::TermDocumentMatrix(corpus)
    return(tm::inspect(my.tdm))
}

AsTermMatrix = function(text, remove.stopwords = TRUE, stoplist = ftaStopList,
  correct.spelling = TRUE, spelling.dictionary = ftaDictionary,
  do.stemming = TRUE, manual.replacements = NULL) {
	word.bag = InitializeWordBag(text, remove.stopwords = remove.stopwords, stoplist = stoplist, correct.spelling = correct.spelling, 
		spelling.dictionary = spelling.dictionary, do.stemming = do.stemming, manual.replacements = manual.replacements)
	return(TermMatrixFromWordBag(word.bag))
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
  correct.spelling = TRUE, spelling.dictionary = ftaDictionary,
  do.stemming = TRUE, manual.replacements = NULL, pos.words = ftaPositiveWords, 
  neg.words = ftaNegativeWords) {
	word.bag = InitializeWordBag(text, remove.stopwords = remove.stopwords, stoplist = stoplist, correct.spelling = correct.spelling, 
		spelling.dictionary = spelling.dictionary, do.stemming = do.stemming, manual.replacements = manual.replacements)
	sentiment.matrix = SentimentScoresFromWordBag(word.bag, pos.words = pos.words, neg.words = neg.words)
}