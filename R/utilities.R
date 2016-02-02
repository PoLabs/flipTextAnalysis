

# Tokenizer for ftaTextAnalysis. Remove all characters which are not alphanumeric,
# or a plus-sign (which is used internally to denote joined words / phases).
# Convert to lower case.
# Split by whitespace.
ftaTokenize <- function(text) {
  #text = lapply(text, gsub, pattern = "[^[:print:]]", replacement = "")

  # Remove all non-aplhanumeric characters, except + which is being used to
  # denote tokens joined as phrases
  text <- lapply(text, gsub, pattern = "[^[:alnum:][:space:]\\+]", replacement = "")

  text <- lapply(text, tolower) # Lower case
  tokenized <- sapply(text, strsplit, split = " ") # Split text by white space
  return(tokenized)
}

# Find token in source.tokens and return corresponding token in target
mapToken <- function(token, source.tokens, target.tokens) {
  if (length(source.tokens) != length(target.tokens)) {
    stop("mapToken: expected source.tokens and target.tokens to be the same length.")
  }
  if (length(unique(source.tokens)) != length(source.tokens)) {
    stop("mapToken: expected elements of source.tokens to be unique.")
  }
  index <- match(token, source.tokens)
  if (is.na(index)) {
    warning(paste("mapToken: token '", token, "' not found in source.tokens", sep = ""))
    return(token)
  }
  return(target.tokens[index])
}

# Return a binary vector indicating which elements of x are in the stop word list
findStopWords <- function(x, stoplist = ftaStopList) {
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
  for (j in 1L:length(tokenized))
  {
    cur_tokes <- tokenized[[j]]
    cur_tokes <- cur_tokes[cur_tokes != ""] #Exclude blank/empty strings and NA entries
    cur_tokes <- cur_tokes[cur_tokes != " "]
    cur_tokes <- cur_tokes[!is.na(cur_tokes)]
    new_cur_tokes <- vector('character', length = length(cur_tokes))
    for (k in seq(cur_tokes))
    {
      new_cur_tokes[k] <- after[which(before == cur_tokes[k])]
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

#' @export
print.tidyText <- function(x, ...)
{
  dd <- data.frame("Original Text" = names(x), "Transformed Text" = as.character(x))
  my.dt <- dataTableWithRItemFormat(dd)
  print(my.dt)
}

# Phrases are represented with '+' joining the words,
# but we don't want to print these out for the user.
makeWordBagTextReadable = function(text)
{
  return(gsub("[+]", " ", text))
}


# Build the datatable and print
dataTableWithRItemFormat <- function(dd)
{
  show.row.names = TRUE
  # Specify the header style information that will be used by datatables to draw the output.
  # For some reason this is handled separately to the style of the cell contents
  header.style <- "th { font-family: 'Segoe UI'; font-weight: bold; color: white; background-color: #5B9BD5; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white;}"

  num.col <- ncol(dd)
  dd$oddoreven <- 1:nrow(dd) %% 2 # Extra dummy column to help us format the table. Will be made invisible later
  col.names <- colnames(dd)
  col.names <- gsub("\\.", " ", col.names)

  # Inspect the classes of the columns to determine whether each is to be left or right aligned.
  column.classes <- lapply(dd, class)
  .isNumericClass <- function (c) {
      return(any(c %in% c("numeric", "integer", "logical")) )
  }
  column.is.numeric <- unlist(lapply(column.classes, .isNumericClass))
  right.align.columns <- unname(which(column.is.numeric))
  left.align.columns <- unname(which(!column.is.numeric))

  column.to.color.by <- num.col + 1
  if (show.row.names)
  {
    header.names <- c(" ", col.names)
    column.to.remove <- num.col + 1
    main.table.format.columns <- 0:num.col + 1
  } else {
    header.names <- col.names
    column.to.remove <- num.col
    main.table.format.columns <- 0:num.col
    right.align.columns <- right.align.columns - 1
    left.align.columns <- left.align.columns - 1
  }
  #cat(header.names)



  # The container parameter allows us to design the header of the table
  # using CSS
  my.container <-  htmltools::withTags(table(
    style(type = "text/css", header.style),
    thead(
      tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
      )
    )
  ))

  my.options <- list(autoWidth = FALSE,
                     columnDefs = list(list(targets = "_all", orderable = FALSE),
                                       list(targets = column.to.remove, visible = FALSE),
                                       list(targets = " ", className = 'dt-center'),
                                       list(targets = left.align.columns, className = 'dt-left'),
                                       list(targets = right.align.columns, className = 'dt-right'))
                    )

  mydt <- DT::datatable(dd,
                        rownames = show.row.names,
                        class = 'hover', # Built-in class with least amount of existing formatting. Have to choose a class
                        container = my.container,
                        options = my.options
                        )

    mydt <- DT::formatStyle(mydt,
                      columns = 1:num.col,
                      valueColumns = column.to.color.by,
                      fontFamily = "Segoe UI",
                      fontSize = "13px",
                      paddingRight = "1em",
                      borderRightWidth = "1px",
                      borderRightStyle = "solid",
                      borderRightColor = "white",
                      borderBottomColor = "rgb(255, 255, 255)",
                      borderBottomStyle = "solid",
                      borderBottomWidth = "1px",
                      borderCollapse = "collapse",
                      marginBottom = "0px",
                      marginLeft = "0px",
                      marginRight = "0px",
                      marginTop = "0px",
                      paddingBottom = "0px",
                      paddingLeft = "5.2px",
                      paddingRight = "13px",
                      paddingTop = "0px",
                      verticalAlign = "middle",
                      wordWrap = "break-word",
                      backgroundColor = DT::styleEqual(c(1,0), c('rgb(234, 243, 250)', 'rgb(207, 226, 243)'))
    )

  # Row names in blue
  if (show.row.names) {
    mydt <- DT::formatStyle(mydt,
                            columns = " ",
                            backgroundColor = "rgb(91, 155, 213)",
                            borderBottomColor = "rgb(255, 255, 255)",
                            borderBottomStyle = "solid",
                            borderBottomWidth = "1px",
                            borderCollapse = "collapse",
                            borderRightColor = "rgb(255, 255, 255)",
                            borderRightStyle = "solid",
                            borderRightWidth = "1px",
                            color = "rgb(255, 255, 255)",
                            cursor = "default",
                            emptyCells = "show",
                            fontFamily = "Segoe UI",
                            fontSize = "13px",
                            fontWeight = "bold",
                            lineHeight = "normal",
                            paddingBottom = "2.6px",
                            paddingLeft = "5.2px",
                            paddingRight = "5.2px",
                            paddingTop = "2.6px",
                            textAlign = "left",
                            verticalAlign = "middle"
    )
  }

  return(mydt)
}
