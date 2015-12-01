#' \code{InitializeWordBag}
#'
#' Create a wordBag object from a vector of strings.
#'
#' @param text A character vector containing the text to be analyzed.
#' @param remove.stopwords A boolean value specifying whether or not to identify stopwords and remove them from subsequent analyses.
#' @param stoplist A character vector containg the stopwords. The default value is this package's built-in stopwords list, ftaStopList.
#' @param correct.spelling A boolean value specifying whether or not to identify spelling mistakes and to generate corrections.
#' @param spelling.dictionary A character vector containing the dictionary to use to check each word for mis-spellings. The default value is this package's built-in english dictionary, ftaDictionary.
#' @param do.stemming A boolean value specifying whether or not to stem the words and replace each stem with the most commonly-occuring word that matches that stem.
#' @param manual.replacements A matrix of characters with two columms. The first column specifies the words to replace, and the second column specifies the corresponding replacements.
#' @param phrases A character vector containing strings that should be treated as single units during processing.
#' @param min.frequency An integer specifiying the smallest frequency of word to keep in the transformed text.
#' @param alphabetical.sort A logical value indicating whether word lists associated with this Word Bag should be printed in alphabetical order or in order of word frequencies.
#' @param print.type A string indicating the type of printing that should be performed when the Word Bag is printed. The two options are "frequencies", which will print the unique tokens next to their frequencies, and "transformations", which will print the original text next to the new transformed text. All printing is done using datatable from package D3.
#' @return An object of class \code{wordBag} containing the word bag details.
#' @export
InitializeWordBag = function(text, remove.stopwords = TRUE, stoplist = ftaStopList,
  operations = c("spelling", "stemming"), spelling.dictionary = ftaDictionary,
  manual.replacements = NULL, phrases = NULL, min.frequency = 1, alphabetical.sort = TRUE, print.type = "frequencies") 
{

# Function to check that the inputs to the wordBag creation make sense
  .checkWordBagOperations <- function(operations, remove.stopwords, stoplist, spelling.dictionary, manual.replacements) 
  {
    
    valid.operations <- c("spelling", "replacement", "stemming", "")

    # Check that the user has entered operations that we support
    invalid.operations <- operations[which(! operations %in% valid.operations)]
    if (length(invalid.operations) > 0) 
    {
      stop(paste(invalid.operations[1], " is not a valid operation. Valid operations are: ", valid.operations, sep = ""))
    }

    # If the user wants to do manual replacements they should
    # specify at least one replacement to make.
    if ("replacement" %in% operations) 
    {
      if (is.null(manual.replacements)) 
      {
        stop("A manual replacement step has been included in the wordbag operations, but no replacements have been specified.")
      }

      # Check dimensions and properties of replacements matrix
      if (class(manual.replacements) != "matrix" 
        || class(manual.replacements[1, ]) != "character" 
        || ncol(manual.replacements) != 2) 
      {
        stop("Manual replacements should be specified as a two-column matrix with entries that are characters (words).")
      }

      # The first column of the replacements can't have duplicates.
      # Which of the duplicates should be mapped?
      duplicates = manual.replacements[duplicated(manual.replacements[, 1]), 1]
      if (length(duplicates) > 0) 
      {
        stop(paste("manual.replacements contains duplicates in the first column, for example: ", duplicates[1], sep = ""))
      }
    }

    if (remove.stopwords) 
    {
      if (is.null(stoplist)) 
      {
        stop("Stopword removal has been used, but no list of stopwords has been provided.")
      }
      if (class(stoplist) != "character") 
      {
        stop("'stoplist' should be a character vector.")
      }
    }

    if ("spelling" %in% operations) 
    {
      if (is.null(spelling.dictionary)) 
      {
        stop("A spelling correction step has been included in the opearations, but no dictionary has been provided.")
      }
      if (class(spelling.dictionary) != "character") 
      {
        stop("'spelling.dictionary' should be a vector of characters.")
      }
    }

    if (!is.null(manual.replacements) && ! "replacement" %in% operations) 
    {
      stop("Replacements have been specified, but the list of operations does not contain a 'replacement' step.")
    }

    return(TRUE)
  }


  # Check that the options supplied make sense
  .checkWordBagOperations(operations = operations, 
                         remove.stopwords = remove.stopwords, 
                         stoplist = stoplist, 
                         spelling.dictionary = spelling.dictionary, 
                         manual.replacements = manual.replacements)


  word.bag = list()
  original.text = text

  # Convert all the text to lowercase first
  # Makes all other steps easier
  text = tolower(text)
  

  # Catch any phrases specified by the user now
  # so that they get treated as a unit.
  # Phrases are two or more words, where each
  # word is joined by a +. Never print the +
  if (!is.null(phrases)) 
  {
    phrases = tolower(phrases)
    text = replacePhrasesInText(text, phrases)
  }



  # Tokenize the text
  tokenized = ftaTokenize(text)

  # Get list of unique tokens and their counts
  tokens.counts = countUniqueTokens(tokenized)
  tokens = tokens.counts$tokens
  counts = tokens.counts$counts

  # If manual replacements have been specified, these words need to
  # be added to the tokens
  if (!is.null(manual.replacements)) {
    # Add entries for any new tokens to the existing vector of tokens
    # and initialize the count to zero
    extra.tokens = as.vector(manual.replacements)
    extra.tokens = unique(extra.tokens)
    
    for (j in 1L:length(extra.tokens)) {
      if (! extra.tokens[j] %in% tokens) {
        current.length = length(tokens)
        tokens[current.length + 1] = extra.tokens[j]
        counts[current.length + 1] = 0
      }
    }
  }

  # These keep track of the tokens as we move through the different processes
  current.tokens = tokens
  current.counts = counts

  word.bag$original.text = original.text
  word.bag$tokens = tokens
  word.bag$counts = counts
  word.bag$tokenized = tokenized
  word.bag$manual.replacements = manual.replacements
  word.bag$alphabetical.sort = alphabetical.sort
  word.bag$min.frequency = min.frequency
  word.bag$print.type = print.type
  class(word.bag) = "wordBag"

  if (remove.stopwords) {
    word.bag$stopwords = findStopWords(current.tokens, stoplist)
  }

  for (j in 1L:length(operations)) {
    op = operations[j]
    if (op == "spelling") {
      spelling.errors = FindSpellingErrors(current.tokens, spelling.dictionary)
      corrected.tokens = GetCorrections(current.tokens, current.counts, spelling.errors, do.not.correct = word.bag$stopwords)
      word.bag$corrected.tokens = corrected.tokens #remove
      word.bag$spelling.corrected = TRUE
      corrected.counts = getUpdatedCounts(current.tokens, current.counts, corrected.tokens)
      word.bag$corrected.counts = corrected.counts #remove
      current.tokens = corrected.tokens
      current.counts = corrected.counts
    } else if (op == "replacement") {
      # Do replacements
      new.tokens = current.tokens
      for (j in 1L:length(tokens)) {
        index = which(manual.replacements[, 1] == new.tokens[j])
        if (length(index) > 0) {
          new.tokens[j] = manual.replacements[index, 2]
        }
      }

      # Update counts
      new.counts = getUpdatedCounts(current.tokens, current.counts, new.tokens)

      # Update our current tokens and counts
      current.tokens = new.tokens
      current.counts = new.counts
    } else if (op == "stemming") {
      stemmed.tokens = GetStemNames(current.tokens, current.counts)
      word.bag$stemmed = TRUE
      word.bag$stemmed.tokens = stemmed.tokens #remove
      stemmed.counts = getUpdatedCounts(current.tokens, current.counts, stemmed.tokens)
      word.bag$stemmed.counts = stemmed.counts #remove
      current.tokens = stemmed.tokens
      current.counts = stemmed.counts
    }
  }

  # Store the tokens that are replacing the original tokens
  word.bag$replace.tokens = current.tokens
  word.bag$replace.counts = current.counts

  # Transform the original text, and re-tokenize
  if (remove.stopwords || length(operations) > 0 || min.frequency > 1 || !is.null(phrases)) 
  {

    replace.tokens = current.tokens
    if (remove.stopwords) {
      # transformed.tokenized = lapply(tokenized, setdiff, y = tokens[word.bag$stopwords == 1])
      replace.tokens[which(word.bag$stopwords == 1)] = ""
    }

    # Remove words which are less frequent that the minimum specified
    if (min.frequency > 1) {
      replace.tokens[which(current.counts < min.frequency)] = ""
    }
    transformed.tokenized = mapTokenizedText(tokenized, before = tokens, after = replace.tokens)
    transformed.text = sapply(transformed.tokenized, paste, collapse = " ")

    # Do a final phrase replacement and get final tokens
    if (!is.null(phrases)) {
      transformed.text = replacePhrasesInText(transformed.text, phrases)
    }

    transformed.tokenized = ftaTokenize(transformed.text)

    tokens.counts = countUniqueTokens(transformed.tokenized)
    current.tokens = tokens.counts$tokens
    current.counts = tokens.counts$counts

    word.bag$transformed.tokenized = transformed.tokenized
    word.bag$transformed.text = transformed.text

  } else {
    word.bag$transformed.tokenized = tokenized
    word.bag$transformed.text = text
  }

  word.bag$final.tokens = current.tokens
  word.bag$final.counts = current.counts

  return(word.bag)
}


  


# Use the package DT to generate nice-looking tables. There are two modes,
# which are specified in the word bag creation. "frequencies" creates a table
# of the words and their frequencies. "transformations" creates a table
# showing the before and after text.
#' @export
print.wordBag <- function(x, ...)
{

  if (x$print.type != "transformations") 
  {
    # Print word frequencies
    if (x$print.type != "frequencies")
    {
      warning(paste("Don't recognise the print type", x$print.type))
    }
    
    tokens <- x$final.tokens
    counts <- x$final.counts
    
    # Sort tokens and counts according to alphabetical
    # order or frequency
    if (x$alphabetical.sort) 
    {
      counts <- counts[order(tokens)]
      tokens <- sort(tokens)
    } else {
      tokens <- tokens[order(counts, decreasing = TRUE)]
      counts <- sort(counts, decreasing = TRUE)
    }

    # Remove words below the frequency threshhold
    tokens <- tokens[counts >= x$min.frequency]
    counts <- counts[counts >= x$min.frequency] 

    dd <- data.frame("Words" = tokens, "Frequencies" = counts)

  } else {
    # Print original and transformed text
    dd <- data.frame("Original Text" = x$original.text, "Transformed Text" = makeWordBagTextReadable(x$transformed.text))
  }

  # Build the datatable and print

  # Specify the header style information that will be used by datatables to draw the output.
  # For some reason this is handled separately to the style of the cell contents
  header.style <- "font-family: 'Lato', 'Helvetica Neue', Helvetica, Arial, sans-serif; font-size: 15.px; text-align: left;"

  num.col <- ncol(dd)
  col.names <- colnames(dd)
  col.names <- gsub("\\.", " ", col.names)
  my.container <-  htmltools::withTags(table(
    thead(
      tr(
          lapply(col.names, th, style = header.style)
      )
    )
  ))
  
  mydt <- DT::datatable(dd, 
                        rownames = FALSE, 
                        class = 'row-border compact hover stripe',
                        container = my.container
                        )

  print(formatStyle(mydt, 
                    columns = 0:(num.col - 1), 
                    fontFamily = "Segoe UI",
                    textAlign = "left"
                    )) 
}
