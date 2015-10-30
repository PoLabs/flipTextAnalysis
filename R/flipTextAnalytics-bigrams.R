

initializeBigramBag = function(wb, map_type = "stem") {
  bigramBag = list()
  # Replace tokenized text with stemmed text or spelling-corrected text
  # depending on which option has been chosen.
  if (map_type == "stem") {
    mapped_tokenized = mapTokenizedText(wb$tokenized, wb$tokens, wb$stemmed.tokens)
  } else {
    if (map_type == "corrected") {
      mapped_tokenized = mapTokenizedText(wb$tokenized, wb$tokens, wb$corrected.tokens)
    } else mapped_tokenized = wb$tokenized
  }

  bigram_tokenized = nGramTokenize(mapped_tokenized, 2)
  trigram_tokenized = nGramTokenize(mapped_tokenized, 3)

  bigrams = vector("character", length = 1000)
  bicounts = vector("integer", length = 1000)
  word_counter = 1
  for (j in 1L:length(bigram_tokenized)) { # Collect and count unique bigrams
    curtokes = bigram_tokenized[[j]]
    if (length(curtokes) > 0) {
      for (k in 1L:length(curtokes)) {
        cur_word = curtokes[k]
        if (nchar(cur_word) > 0) {
          ind = which(bigrams == cur_word)
          if (length(ind) == 0) {
            bigrams[word_counter] = cur_word
            bicounts[word_counter] = 1
            word_counter = word_counter + 1
            if (word_counter >= length(bigrams)) {
              bigrams = c(bigrams,vector("character", length = 1000))
              bicounts = c(bicounts,vector("integer", length = 1000))
            }
          } else bicounts[ind] = bicounts[ind] + 1
        }
      }
    }
  }
  bigrams = bigrams[1:(word_counter-1)]
  bicounts = bicounts[1:(word_counter-1)]
  bicounts = bicounts[order(bigrams)]
  bigrams = sort(bigrams)

  #     trigrams = vector('character',length = 1000)
  #     tricounts = vector("integer",length=1000)
  #     word_counter = 1
  #     for (j in 1L:length(trigram_tokenized)) { # Collect and count unique trigrams
  #         curtokes = trigram_tokenized[[j]]
  #         if (length(curtokes) > 0) {
  #             for (k in 1L:length(curtokes)) {
  #                 cur_word = curtokes[k]
  #                 if (nchar(cur_word) > 0) {
  #                     ind = which(trigrams==cur_word)
  #                     if (length(ind) == 0) {
  #                         trigrams[word_counter] = cur_word
  #                         tricounts[word_counter] = 1
  #                         word_counter = word_counter + 1
  #                         if (word_counter >= length(trigrams)) {
  #                             trigrams = c(trigrams,vector("character",length=1000))
  #                             tricounts = c(tricounts,vector("integer",length=1000))
  #                         }
  #                     } else tricounts[ind] = tricounts[ind] + 1
  #                 }
  #             }
  #         }
  #     }
  #     trigrams = trigrams[1:(word_counter-1)]
  #     tricounts = tricounts[1:(word_counter-1)]
  #     tricounts = tricounts[order(trigrams)]
  #     trigrams = sort(trigrams)

  bigramBag[["bigrams"]] = bigrams
  bigramBag[["bicounts"]] = bicounts
  bigramBag[["bigram_units"]] = sapply(bigrams, strsplit, split = " ")
  #     bigramBag[["trigrams"]] = trigrams
  #     bigramBag[["tricounts"]] = tricounts
  #     bigramBag[["trigram_units"]] = sapply(trigrams,strsplit,split = " ")
  bigramBag[["stopwords"]] = vector("integer", length = length(bigrams))
  bigramBag[["tokenized"]] = bigram_tokenized
  bigramBag[["mapped_tokenized"]] = mapped_tokenized
  bigramBag[["collocations"]] = vector("integer", length = length(bigrams))
  class(bigramBag) = "bigramBag"
  return(bigramBag)
}






findBigramStopwords = function(x, stoplist) {
  y = vector("integer", length = length(x))
  for (j in 1L:length(x)) {
    if(any(x[[j]] %in% stoplist)) {
      y[j] = 1
    }
  }
  return(y)
}


# Use Pearson's chi-squared test to determine if two words in a bigram are independent
# or a collocation (phrase). Collocation specifically means a phrase that contains more
# meaning than can be determined from its individual words separately, but more
# generally it refers to words which appear together more likely than chance.
#
# The output is a binary vector indicating whether or not each bigram from bb$bigrams
# is a collocation or not.
#
# Arguments:
#
# bb - an object of class bigramBag
# wb - an onject of class wordBag
# map.type - a string that tells the function which map has been used to construct the bigrams
#           - "stem" indicates that stemmed words from the wordBag have been used to
#              tokenized the bigram
#           - "corrected" indicated that spell-corrected words have been used.
#           - "none" indicates that raw words have been used.

detectCollocations = function(bb, wb, map.type = "stem", min.count = 10) {
  collocations = vector("integer", length = length(bb$bigrams))
  cs = vector("numeric", length = length(bb$bigrams))
  words = wb$tokens
  if (map.type == "stem") {
    counts = wb$stemmed_counts
  } else {
    if (map.type == "corrected") {
      counts = wb$corrected_counts
    } else {
      counts = wb$counts
    }
  }
  bigrams = bb$bigrams
  units = bb$bigram_units
  bicounts = bb$bicounts
  total_bigram_count = sum(bicounts)
  for (j in 1L:length(bigrams)) {
    if(bicounts[j] < min.count || bb$stopwords[j] == 1) {
      collocations[j] = 0
    } else {
      obs = matrix(0, nrow = 2, ncol = 2)
      obs[1,1] = bicounts[j]
      obs[1,2] = counts[which(words == units[[j]][2])] - obs[1,1]
      obs[2,1] = counts[which(words == units[[j]][1])] - obs[1,1]
      obs[2,2] = total_bigram_count - (obs[1,1] + obs[1,2] + obs[2,1])
      ev = matrix(0, nrow = 2, ncol = 2)
      ev[1,1] = sum(obs[,1]) * sum(obs[1,]) / total_bigram_count
      ev[1,2] = sum(obs[,2]) * sum(obs[1,]) / total_bigram_count
      ev[2,1] = sum(obs[,1]) * sum(obs[2,]) / total_bigram_count
      ev[2,2] = sum(obs[,2]) * sum(obs[2,]) / total_bigram_count

      chisq = (total_bigram_count * (obs[1,1] * obs[2,2] - obs[1,2] * obs[2,1])^2) / ((obs[1,1] + obs[1,2]) * (obs[1,1] + obs[2,1]) * (obs[1,2] + obs[2,2]) * (obs[2,1] + obs[2,2]))
      alpha = 0.95
      count_thresh = 0
      if (all(ev > count_thresh) & obs[1,1] > ev[1,1]) cs[j] = chisq
      if (chisq >= qchisq(alpha, 1) & all(ev > count_thresh) & obs[1,1] > ev[1,1]) {
        collocations[j] = 1
      } else
        collocations[j] = 0
    }
  }
  return(collocations)
}


