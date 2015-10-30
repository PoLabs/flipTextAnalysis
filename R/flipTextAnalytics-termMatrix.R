
makeTermList = function(wb, threshold) {
  if (wb$stemmed) {
    token.list = wb$stemmed.tokens
    counts.used = wb$stemmed.counts
  } else if (wb$spelling.corrected) {
    token.list = wb$corrected.tokens
    counts.used = wb$corrected.counts
  } else {
    token.list = wb$tokens
    counts.used = wb$counts
  }
  term.list = vector("list",length = length(wb$tokenized))
  unique.tokens = unique(token.list[((wb$stopwords == 0) & (counts.used > threshold))])
  print(length(wb$stopwords))
  print(length(counts.used))
  for (j in 1L:length(wb$tokenized)) {
    cur.tokes = wb$tokenized[[j]]
    cur.ind = vector("integer",length = length(cur.tokes))
    counter = 1
    if (length(cur.tokes > 0)) {
      for (k in 1L:length(cur.tokes)) {
        ind = match(cur.tokes[k],unique.tokens)
        if (!is.na(ind)) {
          cur.ind[counter] = ind
          counter = counter + 1
        }
      }
    }
    if (counter > 1) {
      term.list[[j]] = cur.ind[1:(counter-1)]
    } else term.list[[j]] = vector("integer",length=0)
  }
  termList = list(term.list = term.list, unique.tokens = unique.tokens)
  return(termList)
}





makeTermMatrixFromTermList = function(tl, binary = TRUE) {
  term.matrix = matrix(0,nrow = length(tl$term.list), ncol = length(tl$unique.tokens), dimnames = list(NULL, tl$unique.tokens))
  if (binary) {
    for (j in 1L:length(tl$term.list)) {
      if (length(tl$term.list[[j]]) > 0) {
        for (k in 1L:length(tl$term.list[[j]])) {
          if (term.matrix[j,tl$term.list[[j]][k]] == 0) term.matrix[j,tl$term.list[[j]][k]] = 1
        }
      }
    }
  } else {
    for (j in 1L:length(tl$term.list)) {
      if (length(tl$term.list[[j]]) > 0) {
        for (k in 1L:length(tl$term.list[[j]])) {
          term.matrix[j,tl$term.list[[j]][k]] = term.matrix[j,tl$term.list[[j]][k]] + 1
        }
      }
    }
  }
  return(term.matrix)
}





termMatrixFromScratch = function(text, threshold = 5, binary = TRUE, remove.stopwords = TRUE, stoplist = ftaStopList, correct.spelling = TRUE, spelling.dictionary = ftaDictionary, do.stemming = TRUE) {
  myWordBag = initializeWordBag(text, remove.stopwords, stoplist, correct.spelling, spelling.dictionary, do.stemming)
  myTermList = makeTermList(myWordBag, threshold)
  myTermMatrix = makeTermMatrixFromTermList(myTermList, binary)
  return(myTermMatrix)
}

termMatrixFromWordBag = function(word.bag, threshold = 5, binary = TRUE) {
    myTermList = makeTermList(word.bag,threshold)
    myTermMatrix = makeTermMatrixFromTermList(myTermList ,binary)
    return(myTermMatrix)
}


mergeBigramAndUnigramTokenizedText  = function(wb, bb,bigrams,tokens) {
  new.bigram.tokenized = vector("list",length=length(bb$tokenized))
  for (j in 1L:length(bb$tokenized)) {
    cur.tokes = bb$tokenized[[j]]
    counter = 1
    if (length(cur.tokes) > 0) {
      for (k in 1L:length(cur.tokes)) {
        if (any(bigrams == cur.tokes[k])) {
          new.bigram.tokenized[[j]][counter] = cur.tokes[k]
          counter = counter + 1
        }
      }
    }
  }
  final.tokenized = vector("list",length = length(wb$tokenized))
  for (j in 1L:length(final.tokenized)) {
    if (length(bb$mapped.tokenized[[j]]) > 0) {
      cur.tokes = bb$mapped.tokenized[[j]]
      counter = 1
      if (length(cur.tokes) > 1) {
        for (k in 1L:(length(cur.tokes)-1)) {
          if (paste(cur.tokes[k],cur.tokes[k+1],sep = " ") %in% new.bigram.tokenized[[j]]) {
            final.tokenized[[j]][counter] = paste(cur.tokes[k],cur.tokes[k+1],sep = " ")
            bigram.added = TRUE
            counter = counter + 1
          } else {
            final.tokenized[[j]][counter] = cur.tokes[k]
            bigram.added = FALSE
            counter = counter + 1
          }
        }
        if (!bigram.added) {
          final.tokenized[[j]][counter] = cur.tokes[length(cur.tokes)]
        }
      } else if (length(cur.tokes) > 0) {
        final.tokenized[[j]][1] = cur.tokes[1]
      }
    }
  }
  return(final.tokenized)
}

makeTermListWithBigrams = function(wb, bb, threshold) {
  com = combineUnigramsAndBigrams(wb, bb, threshold)
  final.tokens = com$combined.grams[com$combined.counts > threshold]
  bigrams = bb$bigrams[bb$collocations == 1 & bb$bicounts > threshold]
  final.tokenized = mergeBigramAndUnigramTokenizedText(wb, bb, bigrams, final.tokens)
  term.list = vector("list", length = length(final.tokenized))
  for (j in 1L:length(final.tokenized)) {
    cur.tokes = final.tokenized[[j]]
    cur.ind = vector("integer",length = length(cur.tokes))
    counter = 1
    if (length(cur.tokes > 0)) {
      for (k in 1L:length(cur.tokes)) {
        ind = match(cur.tokes[k],final.tokens)
        if (!is.na(ind)) {
          cur.ind[counter] = ind
          counter = counter + 1
        }
      }
    }
    if (counter > 1) {
      term.list[[j]] = cur.ind[1:(counter-1)]
    } else term.list[[j]] = vector("integer",length=0)
  }
  termList = list(term.list = term.list, unique.tokens = final.tokens)
}


bigramTermMatrixFromScratch = function(text, mydict, stoplist, threshold = 2) {
  wb = initializeWordBag(text)
  wb$stopwords = findStopWords(wb$tokens, stoplist)
  wb$spelling.errors = findSpellingErrors(wb$tokens, mydict)
  wb$corrected.tokens = getCorrections(wb)
  wb$spelling.corrected = TRUE
  wb$corrected.counts = getCorrectedCounts(wb)
  wb$map[,2] = getStemNames(wb)
  wb$stemmed = TRUE
  wb$stemmed.counts = getStemmedCounts(wb)
  bb = initializeBigramBag(wb)
  bb$stopwords = findBigramStopwords(bb$bigram.units, stoplist)
  bb$collocations = detectCollocations(bb,wb,"stem")
  tl = makeTermListWithBigrams(wb, bb, threshold)
  tm = makeTermMatrixFromTermList(tl)
  return(tm)
}



# This funnction megres the list of bigrams from bb with the list of unigrams from wb and
# updates the counts of each unigram if it is found within one of the bigrams that are
# being kept. If two bigrams overlap ("clash") then the most frequent one is kept in the
# list while the other is discarded. This is to prevent double-counting.
#
# This function returns the combined list in combined.grams, and the updated counts in
# combined.counts. These two form a two-element list.

combineUnigramsAndBigrams = function(wb, bb, bigram.threshold = 5) {
  full.stemmed.counts = wb$stemmed.counts[wb$stopwords == 0]
  unigrams = wb$map[wb$stopwords == 0,2]
  unigrams = unigrams[full.stemmed.counts > 0]
  unicounts = wb$stemmed.counts[wb$stopwords == 0]
  unicounts = unicounts[full.stemmed.counts > 0]

  bigrams = bb$bigrams[bb$collocations == 1 & bb$bicounts > bigram.threshold]
  bicounts = bb$bicounts[bb$collocations == 1 & bb$bicounts > bigram.threshold]
  units = bb$bigram.units[bb$collocations == 1 & bb$bicounts > bigram.threshold]

  if (length(bigrams) > 0) {
    bigrams.to.keep = rep(1,length = length(bigrams))
    bcm = matrix(0,nrow = length(bigrams),ncol = length(bigrams)) # Bigram clash matrix

    # Identify bigram-bigram clashes
    # When two bigrams clash, remove the less-frequent one from the list (single words remain)
    for (j in 1L:length(bigrams)) {
      for (k in 1L:length(bigrams)) {
        if (units[[j]][2] == units[[k]][1]) {
          merged = paste(units[[j]][1],units[[j]][2],units[[k]][2],sep = " ")
          if (any(bb$trigrams == merged)) {
            bcm[j,k] = 1
            if (bicounts[j] > bicounts[k]) bigrams.to.keep[k] = 0
            else bigrams.to.keep[j] = 0
          }
        }
      }
    }

    bigrams = bigrams[which(bigrams.to.keep == 1)]
    bicounts = bicounts[which(bigrams.to.keep == 1)]
    units = units[which(bigrams.to.keep == 1)]

    combined.grams = c(unigrams,bigrams)
    combined.counts = vector("integer",length = length(unigrams) + length(bigrams))

    for (j in 1L:length(bigrams)) {
      unicounts[which(unigrams == units[[j]][1])] = unicounts[which(unigrams == units[[j]][1])] - bicounts[j]
      unicounts[which(unigrams == units[[j]][2])] = unicounts[which(unigrams == units[[j]][2])] - bicounts[j]
    }

    combined.counts = c(unicounts,bicounts)
  } else {
    combined.grams = unigrams
    combined.counts = unicounts
  }

  return(list(combined.grams = combined.grams, combined.counts = combined.counts))
}







