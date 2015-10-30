getStems = function(x) {
  stems = vector("character", length = length(x))
  for (j in 1L:length(x)) {
    stems[j] = SnowballC::wordStem(x[j])
  }
  return(stems)
}


getStemNames = function(wb) {
  if (wb$spelling.corrected) cur.stems = getStems(wb$corrected.tokens)
  else cur.stems = getStems(wb$tokens)
  unique.stems = unique(cur.stems)
  stem.sets = vector("list", length = length(unique.stems))

  if (wb$spelling.corrected) {
    for (j in 1L:length(stem.sets)) {
      stem.sets[[j]] = wb$corrected.tokens[which(cur.stems == unique.stems[j])]
    }
  } else {
    for (j in 1L:length(stem.sets)) {
      stem.sets[[j]] = wb$tokens[which(cur.stems == unique.stems[j])]
    }
  }
  stem.identifiers = vector("character", length = length(unique.stems))
  for (j in 1L:length(unique.stems)) {
    if (wb$spelling.corrected) {
      max.words = getMostFrequentWords(stem.sets[[j]], wb$corrected.tokens, wb$corrected.counts)
    } else {
      max.words = getMostFrequentWords(stem.sets[[j]], wb$tokens, wb$counts)
    }
    stem.identifiers[j] = max.words[1]
  }
  stem.names = vector("character",length = length(wb$tokens))
  for (j in 1L:length(wb$tokens)) {
    stem.names[j] = stem.identifiers[which(unique.stems == cur.stems[j])]
  }
  return(stem.names)
}

getStemNames = function(tokens, counts) {
  
  # Obtain the stemmed version of each token
  cur.stems = getStems(tokens)
  unique.stems = unique(cur.stems)

  # For each unique stem, collect all of the tokens which map to that stem
  stem.sets = vector("list", length = length(unique.stems))
  for (j in 1L:length(stem.sets)) {
    stem.sets[[j]] = tokens[which(cur.stems == unique.stems[j])]
  }
  
  # For each unique stem, determine which of the tokens it matches is most
  # frequent. If multiple words are equal first in frequency, pick the
  # first one in the list.
  stem.identifiers = vector("character", length = length(unique.stems))
  for (j in 1L:length(unique.stems)) {
    max.words = getMostFrequentWords(stem.sets[[j]], tokens, counts)
    stem.identifiers[j] = max.words[1]
  }

  stem.names = vector("character",length = length(tokens))
  for (j in 1L:length(tokens)) {
    stem.names[j] = stem.identifiers[which(unique.stems == cur.stems[j])]
  }
  return(stem.names)
}
