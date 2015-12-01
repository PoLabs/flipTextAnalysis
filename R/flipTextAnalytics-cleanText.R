#' @export
print.tidyText = function(x, ...) 
{
    dd = data.frame("Original Text" = names(x), "Transformed Text" = as.character(x))
    print(DT::datatable(dd))
}

# Phrases are represented with '+' joining the words,
# but we don't want to print these out for the user.
makeWordBagTextReadable = function(text) 
{
  return(gsub("[+]", " ", text))
}