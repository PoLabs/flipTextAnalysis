getStems <- function(x) {
  library(SnowballC)
  stems <- vector("character", length=length(x))
  for (j in 1L:length(x)) {
    stems[j] <- wordStem(x[j])
  }
  return(stems)
}


getStemNames <- function(wb) {
  if (wb$spelling_corrected) cur_stems <- getStems(wb$map[,1])
  else cur_stems <- getStems(wb$tokens)
  unique_stems <- unique(cur_stems)
  stem_sets <- vector("list", length = length(unique_stems))

  if (wb$spelling_corrected) {
    for (j in 1L:length(stem_sets)) {
      stem_sets[[j]] <- wb$map[,1][which(cur_stems == unique_stems[j])]
    }
  } else {
    for (j in 1L:length(stem_sets)) {
      stem_sets[[j]] <- wb$tokens[which(cur_stems == unique_stems[j])]
    }
  }
  stem_identifiers <- vector("character",length = length(unique_stems))
  for (j in 1L:length(unique_stems)) {
    if (wb$spelling_corrected) {
      max_words <- getMostFrequentWords(stem_sets[[j]],wb$map[,1],wb$corrected_counts)
    } else {
      max_words <- getMostFrequentWords(stem_sets[[j]],wb$tokens,wb$counts)
    }
    stem_identifiers[j] <- max_words[1]
  }
  stem_names <- vector("character",length = length(wb$tokens))
  for (j in 1L:length(wb$tokens)) {
    stem_names[j] <- stem_identifiers[which(unique_stems == cur_stems[j])]
  }
  return(stem_names)
}

getStemmedCounts <- function(wb) {
  if(wb$stemmed){
    stemmed_counts <- wb$counts
    for (j in 1L:length(wb$tokens)) {
      if (wb$tokens[j] != wb$map[j,2]) {
        stemmed_counts[which(wb$tokens == wb$map[j,2])] <- stemmed_counts[j] + stemmed_counts[which(wb$tokens == wb$map[j,2])]
        stemmed_counts[j] <- 0
      }
    }
    return(stemmed_counts)
  } else return(NULL)
}
