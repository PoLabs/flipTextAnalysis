initializeWordBag = function(text, remove.stopwords = TRUE, stopwords = ftaStopList, correct.spelling = TRUE, spelling.dictionary = ftaDictionary, do.stemming = TRUE) {
  wordBag = list()
  tokenized = tokenize(text)
  tokens = vector("character", length = 1000)
  counts = vector("integer", length = 1000)
  word_counter = 1
  for (j in 1L:length(tokenized)) { # Collect and count unique tokens
    curtokes = tokenized[[j]]
    if (length(curtokes) > 0) {
      for (k in 1L:length(curtokes)) {
        cur_word = curtokes[k]
        if (nchar(cur_word) > 0 & !is.na(cur_word)) {
          ind = which(tokens == cur_word)
          if (length(ind) == 0) {
            tokens[word_counter] = cur_word
            counts[word_counter] = 1
            word_counter = word_counter + 1
            if (word_counter >= length(tokens)) {
              tokens = c(tokens, vector("character", length = 1000))
              counts = c(counts, vector("integer", length = 1000))
            }
          } else counts[ind] = counts[ind] + 1
        }
      }
    }
  }
  tokens = tokens[1:(word_counter - 1)]
  counts = counts[1:(word_counter - 1)]
  counts = counts[order(tokens)]
  tokens = sort(tokens)
  wordBag[["tokens"]] = tokens
  wordBag[["counts"]] = counts
  wordBag[["tokenized"]] = tokenized
  wordBag[["word_lengths"]] = sapply(tokens, nchar)
  wordBag[["spelling_errors"]] = vector("integer", length = length(tokens))
  wordBag[["map"]] = matrix("character", nrow = length(tokens), ncol = 2)
  wordBag[["stopwords"]] = vector("integer", length = length(tokens))
  wordBag[["corrected_counts"]] = vector("integer", length = length(tokens))
  wordBag[["stemmed_counts"]] = vector("integer", length = length(tokens))
  wordBag[["spelling_corrected"]] = FALSE
  wordBag[["stemmed"]] = FALSE
  class(wordBag) = "wordBag"
  return(wordBag)
}
