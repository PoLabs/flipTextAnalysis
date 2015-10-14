
makeTermList = function(wb, threshold) {
  if (wb$stemmed) {
    token_list = wb$map[,2]
    counts_used = wb$stemmed_counts
  } else if (wb$spelling_corrected) {
    token_list = wb$map[,1]
    counts_used = wb$corrected_counts
  } else {
    token_list = wb$tokens
    counts_used = wb$counts
  }
  term_list = vector("list",length = length(wb$tokenized))
  unique_tokens = unique(token_list[((wb$stopwords == 0) & (counts_used > threshold))])
  print(length(wb$stopwords))
  print(length(counts_used))
  for (j in 1L:length(wb$tokenized)) {
    cur_tokes = wb$tokenized[[j]]
    cur_ind = vector("integer",length = length(cur_tokes))
    counter = 1
    if (length(cur_tokes > 0)) {
      for (k in 1L:length(cur_tokes)) {
        ind = match(cur_tokes[k],unique_tokens)
        if (!is.na(ind)) {
          cur_ind[counter] = ind
          counter = counter + 1
        }
      }
    }
    if (counter > 1) {
      term_list[[j]] = cur_ind[1:(counter-1)]
    } else term_list[[j]] = vector("integer",length=0)
  }
  termList = list(term_list = term_list, unique_tokens = unique_tokens)
  return(termList)
}





makeTermMatrixFromTermList = function(tl, binary = TRUE) {
  term_matrix = matrix(0,nrow = length(tl$term_list), ncol = length(tl$unique_tokens), dimnames = list(NULL, tl$unique_tokens))
  if (binary) {
    for (j in 1L:length(tl$term_list)) {
      if (length(tl$term_list[[j]]) > 0) {
        for (k in 1L:length(tl$term_list[[j]])) {
          if (term_matrix[j,tl$term_list[[j]][k]] == 0) term_matrix[j,tl$term_list[[j]][k]] = 1
        }
      }
    }
  } else {
    for (j in 1L:length(tl$term_list)) {
      if (length(tl$term_list[[j]]) > 0) {
        for (k in 1L:length(tl$term_list[[j]])) {
          term_matrix[j,tl$term_list[[j]][k]] = term_matrix[j,tl$term_list[[j]][k]] + 1
        }
      }
    }
  }
  return(term_matrix)
}





termMatrixFromScratch = function(text,threshold, binary = TRUE) {
  myWordBag = initializeWordBag(text)
  myWordBag$stopwords = findStopWords(myWordBag$tokens,stoplist)

  myWordBag$spelling_errors = findSpellingErrors(myWordBag$tokens,mydict)

  myWordBag$map[,1] = getCorrections(myWordBag)
  myWordBag$spelling_corrected = TRUE
  myWordBag$corrected_counts = getCorrectedCounts(myWordBag)


  myWordBag$map[,2] = getStemNames(myWordBag)
  myWordBag$stemmed = TRUE
  myWordBag$stemmed_counts = getStemmedCounts(myWordBag)


  myTermList = makeTermList(myWordBag,threshold)

  myTermMatrix = makeTermMatrixFromTermList(myTermList,binary)

  return(myTermMatrix)
}



mergeBigramAndUnigramTokenizedText  = function(wb, bb,bigrams,tokens) {
  new_bigram_tokenized = vector("list",length=length(bb$tokenized))
  for (j in 1L:length(bb$tokenized)) {
    cur_tokes = bb$tokenized[[j]]
    counter = 1
    if (length(cur_tokes) > 0) {
      for (k in 1L:length(cur_tokes)) {
        if (any(bigrams == cur_tokes[k])) {
          new_bigram_tokenized[[j]][counter] = cur_tokes[k]
          counter = counter + 1
        }
      }
    }
  }
  final_tokenized = vector("list",length = length(wb$tokenized))
  for (j in 1L:length(final_tokenized)) {
    if (length(bb$mapped_tokenized[[j]]) > 0) {
      cur_tokes = bb$mapped_tokenized[[j]]
      counter = 1
      if (length(cur_tokes) > 1) {
        for (k in 1L:(length(cur_tokes)-1)) {
          if (paste(cur_tokes[k],cur_tokes[k+1],sep = " ") %in% new_bigram_tokenized[[j]]) {
            final_tokenized[[j]][counter] = paste(cur_tokes[k],cur_tokes[k+1],sep = " ")
            bigram_added = TRUE
            counter = counter + 1
          } else {
            final_tokenized[[j]][counter] = cur_tokes[k]
            bigram_added = FALSE
            counter = counter + 1
          }
        }
        if (!bigram_added) {
          final_tokenized[[j]][counter] = cur_tokes[length(cur_tokes)]
        }
      } else if (length(cur_tokes) > 0) {
        final_tokenized[[j]][1] = cur_tokes[1]
      }
    }
  }
  return(final_tokenized)
}

makeTermListWithBigrams = function(wb, bb, threshold) {
  com = combineUnigramsAndBigrams(wb, bb, threshold)
  final_tokens = com$combined_grams[com$combined_counts > threshold]
  bigrams = bb$bigrams[bb$collocations == 1 & bb$bicounts > threshold]
  final_tokenized = mergeBigramAndUnigramTokenizedText(wb, bb, bigrams, final_tokens)
  term_list = vector("list", length = length(final_tokenized))
  for (j in 1L:length(final_tokenized)) {
    cur_tokes = final_tokenized[[j]]
    cur_ind = vector("integer",length = length(cur_tokes))
    counter = 1
    if (length(cur_tokes > 0)) {
      for (k in 1L:length(cur_tokes)) {
        ind = match(cur_tokes[k],final_tokens)
        if (!is.na(ind)) {
          cur_ind[counter] = ind
          counter = counter + 1
        }
      }
    }
    if (counter > 1) {
      term_list[[j]] = cur_ind[1:(counter-1)]
    } else term_list[[j]] = vector("integer",length=0)
  }
  termList = list(term_list = term_list, unique_tokens = final_tokens)
}


bigramTermMatrixFromScratch = function(text,mydict,stoplist,threshold = 2) {
  wb = initializeWordBag(text)
  wb$stopwords = findStopWords(wb$tokens,stoplist)
  wb$spelling_errors = findSpellingErrors(wb$tokens,mydict)
  wb$map[,1] = getCorrections(wb)
  wb$spelling_corrected = TRUE
  wb$corrected_counts = getCorrectedCounts(wb)
  wb$map[,2] = getStemNames(wb)
  wb$stemmed = TRUE
  wb$stemmed_counts = getStemmedCounts(wb)
  bb = initializeBigramBag(wb)
  bb$stopwords = findBigramStopwords(bb$bigram_units,stoplist)
  bb$collocations = detectCollocations(bb,wb,"stem")
  tl = makeTermListWithBigrams(wb,bb,threshold)
  tm = makeTermMatrixFromTermList(tl)
  return(tm)
}



# This funnction megres the list of bigrams from bb with the list of unigrams from wb and
# updates the counts of each unigram if it is found within one of the bigrams that are
# being kept. If two bigrams overlap ("clash") then the most frequent one is kept in the
# list while the other is discarded. This is to prevent double-counting.
#
# This function returns the combined list in combined_grams, and the updated counts in
# combined_counts. These two form a two-element list.

combineUnigramsAndBigrams = function(wb, bb, bigram_threshold = 5) {
  full_stemmed_counts = wb$stemmed_counts[wb$stopwords == 0]
  unigrams = wb$map[wb$stopwords == 0,2]
  unigrams = unigrams[full_stemmed_counts > 0]
  unicounts = wb$stemmed_counts[wb$stopwords == 0]
  unicounts = unicounts[full_stemmed_counts > 0]

  bigrams = bb$bigrams[bb$collocations == 1 & bb$bicounts > bigram_threshold]
  bicounts = bb$bicounts[bb$collocations == 1 & bb$bicounts > bigram_threshold]
  units = bb$bigram_units[bb$collocations == 1 & bb$bicounts > bigram_threshold]

  if (length(bigrams) > 0) {
    bigrams_to_keep = rep(1,length = length(bigrams))
    bcm = matrix(0,nrow = length(bigrams),ncol = length(bigrams)) # Bigram clash matrix

    # Identify bigram-bigram clashes
    # When two bigrams clash, remove the less-frequent one from the list (single words remain)
    for (j in 1L:length(bigrams)) {
      for (k in 1L:length(bigrams)) {
        if (units[[j]][2] == units[[k]][1]) {
          merged = paste(units[[j]][1],units[[j]][2],units[[k]][2],sep = " ")
          if (any(bb$trigrams == merged)) {
            bcm[j,k] = 1
            if (bicounts[j] > bicounts[k]) bigrams_to_keep[k] = 0
            else bigrams_to_keep[j] = 0
          }
        }
      }
    }

    bigrams = bigrams[which(bigrams_to_keep == 1)]
    bicounts = bicounts[which(bigrams_to_keep == 1)]
    units = units[which(bigrams_to_keep == 1)]

    combined_grams = c(unigrams,bigrams)
    combined_counts = vector("integer",length = length(unigrams) + length(bigrams))

    for (j in 1L:length(bigrams)) {
      unicounts[which(unigrams == units[[j]][1])] = unicounts[which(unigrams == units[[j]][1])] - bicounts[j]
      unicounts[which(unigrams == units[[j]][2])] = unicounts[which(unigrams == units[[j]][2])] - bicounts[j]
    }

    combined_counts = c(unicounts,bicounts)
  } else {
    combined_grams = unigrams
    combined_counts = unicounts
  }

  return(list(combined_grams = combined_grams, combined_counts = combined_counts))
}







