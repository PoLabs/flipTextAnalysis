getMostFrequentWords <- function(x,words,counts) {
  cc <- counts[words%in%x]
  ww <- words[words%in%x]
  max_count <- max(cc)
  max_words <- ww[which(cc==max_count)]
  return(max_words)
}


tokenize <- function(text) {
  #text <- lapply(text,gsub, pattern = "[‘’````[:punct:][:cntrl:][:digit:]]",replacement = "") # Remove punctuation
  text <- lapply(text, gsub, pattern = "[^[:print:]]", replacement = "")
  text <- lapply(text, gsub, pattern = "[[:punct:]]", replacement = "")
  text <- lapply(text,tolower) # Lower case
  tokenized <- sapply(text,strsplit,split = " ") # Split text by white space
  return(tokenized)
}





# Find token in source.tokens and return corresponding token in target
mapToken = function(token, source.tokens, target.tokens) {
  if (length(source.tokens) != length(target.tokens)) {
    stop("mapToken: expected source.tokens and target.tokens to be the same length.")
  }
  if (length(unique(source.tokens)) != length(source.tokens)) {
    stop("mapToken: expected elements of source.tokens to be unique.")
  }
  index = match(token, source.tokens)
  if (is.na(index)) {
    warning(paste("mapToken: token '", token, "' not found in source.tokens"))
    return(token)
  }
  return(target.tokens[index])
}



# Return a binary vector indicating which elements of x are in the stop word list
findStopWords <- function(x, stoplist) {
  if (class(stoplist) != "character") {
    stop(paste("findStopWords: Expected stoplist to be a character verctor, instead got a: ", class(stoplist)))
  }
  y <- vector("integer", length = length(x))
  for (j in 1L:length(x)) {
    if (x[j]%in%stoplist) y[j] <- 1
  }
  return(y)
}

mapTokenizedText = function(tokenized,before,after) {
  new_tokenized = vector("list",length = length(tokenized))
  for (j in 1L:length(tokenized)) {
    cur_tokes = tokenized[[j]]
    cur_tokes = cur_tokes[cur_tokes != ""] #Exclude blank/empty strings and NA entries
    cur_tokes = cur_tokes[cur_tokes != " "]
    cur_tokes = cur_tokes[!is.na(cur_tokes)]
    new_cur_tokes = vector('character',length = length(cur_tokes))
    for (k in seq(cur_tokes)) {
      new_cur_tokes[k] = after[which(before == cur_tokes[k])]
    }
    new_tokenized[[j]] = new_cur_tokes
  }
  return(new_tokenized)
}



nGramTokenize = function(tokenized,n) {
  ngram_tokenized = vector("list",length=length(tokenized))
  for (j in 1L:length(tokenized)) {
    ngram_tokenized[[j]] = getnGrams(tokenized[[j]],n)
  }
  return(ngram_tokenized)
}



getnGrams = function(x,n) {
  x = x[which(x != " ")] #remove any blank tokens that have crept in
  x = x[which(x != "")]
  if(length(x) > (n-1)){
    y = vector("character",length = length(x) - n + 1)
    for (j in 1L:length(y)) {
      y[j] = paste(x[j:(j+n-1)],collapse = ' ')
    }
    return(y)
  } else return("")
}


nGramsContaining = function(ngram_units,word) {
  results = vector("integer",length = length(ngram_units))
  for (j in 1L:length(ngram_units)) {
    results[j] = any(ngram_units[[j]] == word)
  }
  return(results)
}


