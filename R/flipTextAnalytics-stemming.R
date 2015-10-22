getStems = function(x) {
  stems = vector("character", length = length(x))
  for (j in 1L:length(x)) {
    stems[j] = SnowballC::wordStem(x[j])
  }
  return(stems)
}


getStemNames = function(wb) {
  if (wb$spelling.corrected) cur.stems = getStems(wb$map[,1])
  else cur.stems = getStems(wb$tokens)
  unique.stems = unique(cur.stems)
  stem.sets = vector("list", length = length(unique.stems))

  if (wb$spelling.corrected) {
    for (j in 1L:length(stem.sets)) {
      stem.sets[[j]] = wb$map[,1][which(cur.stems == unique.stems[j])]
    }
  } else {
    for (j in 1L:length(stem.sets)) {
      stem.sets[[j]] = wb$tokens[which(cur.stems == unique.stems[j])]
    }
  }
  stem.identifiers = vector("character",length = length(unique.stems))
  for (j in 1L:length(unique.stems)) {
    if (wb$spelling.corrected) {
      max.words = getMostFrequentWords(stem.sets[[j]],wb$map[,1],wb$corrected.counts)
    } else {
      max.words = getMostFrequentWords(stem.sets[[j]],wb$tokens,wb$counts)
    }
    stem.identifiers[j] = max.words[1]
  }
  stem.names = vector("character",length = length(wb$tokens))
  for (j in 1L:length(wb$tokens)) {
    stem.names[j] = stem.identifiers[which(unique.stems == cur.stems[j])]
  }
  return(stem.names)
}

getStemmedCounts = function(wb) {
  if(wb$stemmed){
    stemmed.counts = wb$counts
    for (j in 1L:length(wb$tokens)) {
      if (wb$tokens[j] != wb$map[j,2]) {
        stemmed.counts[which(wb$tokens == wb$map[j,2])] = stemmed.counts[j] + stemmed.counts[which(wb$tokens == wb$map[j,2])]
        stemmed.counts[j] = 0
      }
    }
    return(stemmed.counts)
  } else return(NULL)
}
