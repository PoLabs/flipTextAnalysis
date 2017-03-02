

# Tokenizer for ftaTextAnalysis. Remove all characters which are not alphanumeric,
# or a plus-sign (which is used internally to denote joined words / phases).
# Convert to lower case.
# Split by whitespace.
ftaTokenize <- function(text)
{
  #text = lapply(text, gsub, pattern = "[^[:print:]]", replacement = "")

  # Remove apostrophes
  text <- lapply(text, gsub, pattern = "'", replacement = "")
  # Remove all non-alphanumeric characters, except + which is being used to
  # denote tokens joined as phrases
  text <- lapply(text, gsub, pattern = "[^[:alnum:][:space:]\\+]", replacement = " ")

  text <- lapply(text, tolower) # Lower case
  tokenized <- sapply(text, strsplit, split = "[[:space:]]") # Split text by white space
  tokenized <- lapply(tokenized, removeBlankTokens)
  return(tokenized)
}

# Given a vector of strings, return those which are not empty
removeBlankTokens <- function(tokens)
{
    return(tokens[which(nchar(tokens) > 0)])
}

# Find token in source.tokens and return corresponding token in target.tokens
mapToken <- function(token, source.tokens, target.tokens)
{
  if (length(source.tokens) != length(target.tokens))
{
    stop("mapToken: expected source.tokens and target.tokens to be the same length.")
  }
  if (length(unique(source.tokens)) != length(source.tokens)) {
    stop("mapToken: expected elements of source.tokens to be unique.")
  }
  index <- match(token, source.tokens)
  if (is.na(index)) {
    #warning(paste("mapToken: token '", token, "' not found in source.tokens", sep = ""))
    return(token)
  }
  return(target.tokens[index])
}

# Return a binary vector indicating which elements of x are in the stop word list
findStopWords <- function(x, stoplist = get("ftaStopList")) {
  if (class(stoplist) != "character") {
    stop(paste("findStopWords: Expected stoplist to be a character verctor, instead got a: ", class(stoplist)))
  }
  y <- vector("integer", length = length(x))
  for (j in 1L:length(x)) {
    if (x[j]%in%stoplist) y[j] <- 1
  }
  return(y)
}

# Given a list of tokenized text (vectors of words created by ftaTokenize) map the
# tokens according to the word lists before and after.
mapTokenizedText <- function(tokenized, before, after)
{
  new_tokenized <- vector("list", length = length(tokenized))

  # Loop over the tokenized versions of the text documents
  for (j in 1L:length(tokenized))
  {

    # Get all of the non-blank elements of the current tokenized text.
    cur_tokes <- tokenized[[j]]
    cur_tokes <- cur_tokes[cur_tokes != ""] #Exclude blank/empty strings and NA entries
    cur_tokes <- cur_tokes[cur_tokes != " "]
    cur_tokes <- cur_tokes[!is.na(cur_tokes)]

    # For each token in the current tokenized text, look up it's position in "before"
    # and replace it with the corresponding word in "after". If the word does not
    # exist in "before" then leave the word unchanged.
    new_cur_tokes <- vector('character', length = length(cur_tokes))
    for (k in seq(cur_tokes))
    {
      token.match.index <- which(before == cur_tokes[k])
      if (length(token.match.index) > 0)
      {
        new_cur_tokes[k] <- after[token.match.index]
      } else {
        new_cur_tokes[k] = cur_tokes[k]
      }
    }
    new_cur_tokes <- new_cur_tokes[new_cur_tokes != ""]
    new_tokenized[[j]] <- new_cur_tokes
  }
  return(new_tokenized)
}

getUpdatedCounts <- function(initial.tokens, initial.counts, mapped.tokens)
{
  if (class(initial.tokens) != "character")
  {
    stop("getUpdatedCounts: expected 'initial.tokens' to be a character vector.")
  }
  if (class(mapped.tokens) != "character")
  {
    stop("getUpdatedCounts: expected 'mapped.tokens' to be a character vector.")
  }
  if (class(initial.counts) != "numeric")
  {
    stop("getUpdatedCounts: expected 'initial.counts' to be a numeric vector.")
  }
  if (length(initial.tokens) != length(mapped.tokens) || length(initial.counts) != length(mapped.tokens))
  {
    stop("getUpdatedCounts: expected all inputs to be the same length")
  }

  mapped.counts <- initial.counts
  for (j in 1L:length(initial.tokens))
  {
    if (initial.tokens[j] != mapped.tokens[j])
    {
      mapped.counts[which(initial.tokens == mapped.tokens[j])] = mapped.counts[j] + mapped.counts[which(initial.tokens == mapped.tokens[j])]
      mapped.counts[j] = 0
    }
  }
  return(mapped.counts)
}

# Give a list of tokenized text (created for example by ftaTokenize)
countUniqueTokens <- function(tokenized) {
  tokens <- vector("character", length = 1000)
  counts <- vector("integer", length = 1000)
  word_counter <- 1
   # Collect and count unique tokens
  for (j in 1L:length(tokenized))
  {
    curtokes <- tokenized[[j]]
    if (length(curtokes) > 0)
    {
      for (k in 1L:length(curtokes))
      {
        cur.word <- curtokes[k]
        if (nchar(cur.word) > 0 & !is.na(cur.word))
        {
          ind <- which(tokens == cur.word)
          if (length(ind) == 0)
          {
            tokens[word_counter] <- cur.word
            counts[word_counter] <- 1
            word_counter <- word_counter + 1
            if (word_counter >= length(tokens))
            {
              tokens <- c(tokens, vector("character", length = 1000))
              counts <- c(counts, vector("integer", length = 1000))
            }
          } else {
            counts[ind] <- counts[ind] + 1
          }
        }
      }
    }
  }

  # Trim the vectors and sort alphabetically
  tokens <- tokens[1:(word_counter - 1)]
  counts <- counts[1:(word_counter - 1)]
  counts <- counts[order(tokens)]
  tokens <- sort(tokens)
  return(list(tokens = tokens, counts = counts))
}

#' @importFrom flipFormat DataTableWithRItemFormat
#' @export
print.tidyText <- function(x, ...)
{
  dd <- data.frame("Original Text" = names(x), "Transformed Text" = as.character(x))
  my.dt <- DataTableWithRItemFormat(dd)
  print(my.dt)
}

# Phrases are represented with '+' joining the words,
# but we don't want to print these out for the user.
makeWordBagTextReadable = function(text)
{
  return(gsub("[+]", " ", text))
}


