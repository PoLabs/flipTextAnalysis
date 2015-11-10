TermMatrixFromWordBag = function(word.bag, min.frequency = 5) {
    my.tdm = termMatrixFromText(word.bag$transformed.text, min.frequency)
    colnames(my.tdm) = makeWordBagTextReadable(colnames(my.tdm))
    return(my.tdm)
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
  return(invisible(tm::inspect(my.tdm)))
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

MostFrequentWords = function(x, min.frequency = 2, alphabetical = NULL) 
{
    if (class(x) == "wordBag") 
    {
        tokens = x$final.tokens
        counts = x$final.counts     
    } else if (class(x) == "character") {
        tokenized = Tokenize(x)
        tokens.counts = CountUniqueTokens(tokenized)
        tokens = tokens.counts$tokens
        counts = tokens.counts$counts
    }
    if (is.null(alphabetical))
    {
        if (class(x) == "wordBag")
        {
            alphabetical = x$alphabetical.sort    
        } else {
            alphabetical = FALSE
        }
    }

    tokens = tokens[counts >= min.frequency]
    counts = counts[counts >= min.frequency] 

    if (alphabetical) 
    {
        counts = counts[order(tokens)]
        tokens = sort(tokens)
    } else {
        tokens = tokens[order(counts, decreasing = TRUE)]
        counts = sort(counts, decreasing = TRUE)
    }
    
    names(counts) = makeWordBagTextReadable(tokens)
    return(counts)
}