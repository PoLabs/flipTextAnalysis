
# Return the words in x which occur most frequently
# according to counts. Multiple words are returned if
# they have the same frequency.
GetMostFrequentWords = function(x, words, counts) {
  cc = counts[words%in%x]
  ww = words[words%in%x]
  max_count = max(cc)
  max_words = ww[which(cc==max_count)]
  return(max_words)
}


Tokenize = function(text) {
  #text = lapply(text,gsub, pattern = "[‘’````[:punct:][:cntrl:][:digit:]]",replacement = "") # Remove punctuation
  text = lapply(text, gsub, pattern = "[^[:print:]]", replacement = "")
  text = lapply(text, gsub, pattern = "[[:punct:]]", replacement = "")
  text = lapply(text,tolower) # Lower case
  tokenized = sapply(text, strsplit, split = " ") # Split text by white space
  return(tokenized)
}





# Find token in source.tokens and return corresponding token in target
MapToken = function(token, source.tokens, target.tokens) {
  if (length(source.tokens) != length(target.tokens)) {
    stop("mapToken: expected source.tokens and target.tokens to be the same length.")
  }
  if (length(unique(source.tokens)) != length(source.tokens)) {
    stop("mapToken: expected elements of source.tokens to be unique.")
  }
  index = match(token, source.tokens)
  if (is.na(index)) {
    warning(paste("mapToken: token '", token, "' not found in source.tokens", sep = ""))
    return(token)
  }
  return(target.tokens[index])
}



# Return a binary vector indicating which elements of x are in the stop word list
FindStopWords = function(x, stoplist = ftaStopList) {
  if (class(stoplist) != "character") {
    stop(paste("FindStopWords: Expected stoplist to be a character verctor, instead got a: ", class(stoplist)))
  }
  y = vector("integer", length = length(x))
  for (j in 1L:length(x)) {
    if (x[j]%in%stoplist) y[j] = 1
  }
  return(y)
}

RemoveWords = function(tokens, remove.words) {
  if (class(tokens) != "character") {
    stop("RemoveWords: expected 'tokens' to be a character vector.")
  }
  if (class(remove.words) != "character") {
    stop("RemoveWords: expected 'remove.words' to be a character vector.")
  }

  return(setdiff(tokens, remove.words))
}

MapTokenizedText = function(tokenized, before, after) {
  new_tokenized = vector("list", length = length(tokenized))
  for (j in 1L:length(tokenized)) {
    cur_tokes = tokenized[[j]]
    cur_tokes = cur_tokes[cur_tokes != ""] #Exclude blank/empty strings and NA entries
    cur_tokes = cur_tokes[cur_tokes != " "]
    cur_tokes = cur_tokes[!is.na(cur_tokes)]
    new_cur_tokes = vector('character', length = length(cur_tokes))
    for (k in seq(cur_tokes)) {
      new_cur_tokes[k] = after[which(before == cur_tokes[k])]
    }
    new_cur_tokes = new_cur_tokes[new_cur_tokes != ""]
    new_tokenized[[j]] = new_cur_tokes
  }
  return(new_tokenized)
}


GetUpdatedCounts = function(initial.tokens, initial.counts, mapped.tokens) {
  if (class(initial.tokens) != "character") {
    stop("GetUpdatedCounts: expected 'initial.tokens' to be a character vector.")
  }
  if (class(mapped.tokens) != "character") {
    stop("GetUpdatedCounts: expected 'mapped.tokens' to be a character vector.")
  }
  if (class(initial.counts) != "numeric") {
    stop("GetUpdatedCounts: expected 'initial.counts' to be a numeric vector.")
  }
  if (length(initial.tokens) != length(mapped.tokens) || length(initial.counts) != length(mapped.tokens)) {
    stop("GetUpdatedCounts: expected all inputs to be the same length")
  }

  mapped.counts = initial.counts
  for (j in 1L:length(initial.tokens)) {
    if (initial.tokens[j] != mapped.tokens[j]) {
      mapped.counts[which(initial.tokens == mapped.tokens[j])] = mapped.counts[j] + mapped.counts[which(initial.tokens == mapped.tokens[j])]
      mapped.counts[j] = 0
    }
  }
  return(mapped.counts)
}

CountUniqueTokens = function(tokenized) {
  tokens = vector("character", length = 1000)
  counts = vector("integer", length = 1000)
  word_counter = 1
   # Collect and count unique tokens
  for (j in 1L:length(tokenized)) {
    curtokes = tokenized[[j]]
    if (length(curtokes) > 0) {
      for (k in 1L:length(curtokes)) {
        cur.word = curtokes[k]
        if (nchar(cur.word) > 0 & !is.na(cur.word)) {
          ind = which(tokens == cur.word)
          if (length(ind) == 0) {
            tokens[word_counter] = cur.word
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
  
  # Trim the vectors and sort alphabetically
  tokens = tokens[1:(word_counter - 1)]
  counts = counts[1:(word_counter - 1)]
  counts = counts[order(tokens)]
  tokens = sort(tokens)
  return(list(tokens = tokens, counts = counts))
}