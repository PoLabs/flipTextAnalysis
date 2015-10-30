wordBagreplacement = function(x, word.bag) {
    if (word.bag$stemmed) {
        final.tokens = word.bag$stemmed.tokens
    } else if (word.bag$spelling.corrected) {
        final.tokens = word.bag$corrected.tokens
    }
    replacement.matrix = cbind(word.bag$tokens, final.tokens)
    return(stringReplacement(x, replacement.matrix))
}

stringReplacement = function(x, replacement.matrix) {
    tokens = unlist(strsplit(x, split = " ", fixed = TRUE))
    for (j in 1L:length(tokens)) {
        index = which(replacement.matrix[, 1] == tokens[j])
        if (length(index) > 0) {
            if (length(index) > 1) {
                warning(paste("stringReplacementTransformer: multiple entries for: ", tokens[j], ", using first replacement:", replacement.matrix[index, 2]))
            }
            tokens[j] = replacement.matrix[index, 2]
        }
    }
    new_string = paste(tokens, collapse = " ")
    new_string = stringr::str_trim(new_string)
    return(new_string)
}
