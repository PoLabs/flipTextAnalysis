# Function to identify tokens as positive, negative, or neutral.
# Does not handle negation - this should be done later one when looking at
# the context of the token.
tagSentiment = function(tokens, pos.words, neg.words) {
  tagger = function(token) {
    if (token %in% pos.words) {
      return("[pos]")
    } else if (token %in% neg.words) {
      return("[neg]")
    } else {
      return("[neu]")
    }
  }
  return(sapply(tokens, tagger))
}


scoreSentimentForString = function(string, tokens, sentiment.tags) {
  negation.unigrams = c("no", "not", "wasnt", "werent", "wouldnt", "couldnt", "didnt", "never", "nobody", "nothing", "dont")
  negation.bigrams = c("not very")

  current.tokens = unlist(tokenize(string))
  # nothing left after tokenization
  if (length(current.tokens) == 0) {
    return(c(0,0))
  }
  #print(paste("current string: '", string, "'"))
  current.tags = sapply(current.tokens, mapToken, source.tokens = tokens, target.tokens = sentiment.tags)
  positive.score = 0
  negative.score = 0

  # Loop through the tokens and tags.
  # Count positive and negative tokens whilst
  # looking at preceding terms for negations
  for (j in 1L:length(current.tokens)) {
    if (current.tags[j] == "[pos]" || current.tags[j] == "[neg]") {
      is.negated = FALSE
      # Look for preceding unigrams
      if (j > 1) {
        if (current.tokens[j-1] %in% negation.unigrams) {
          is.negated = TRUE
        }
      }

      # If not already negated, look for preceding bigrams
      if (!is.negated && j > 2) {
        if (paste(current.tokens[j-2], current.tokens[j-1]) %in% negation.bigrams) {
          is.negated = TRUE
        }
      }

      # Increment the score appropriately
      if ( (!is.negated && current.tags[j] == "[pos]") || (is.negated && current.tags[j] == "[neg]") ) {
        positive.score = positive.score + 1
      } else {
        negative.score = negative.score + 1
      }
    }
  }
  return(c(positive.score, negative.score))
}
