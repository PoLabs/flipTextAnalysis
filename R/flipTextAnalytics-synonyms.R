
# getAllSynonyms = function(string, terms) {
#     require(qdap)
#     syns = unique(unlist(synonyms(string)))
#     return(c(string, syns[which(syns %in% terms)]))
# }

# getCorrelation = function(string1, string2, correlation.matrix) {
#     terms = colnames(correlation.matrix)
#     ind1 = which(terms == string1, arr.ind = TRUE)
#     ind2 = which(terms == string2, arr.ind = TRUE)
#     return(correlation.matrix[ind1, ind2])
# }

# getCorrelationScoresForTerms = function(term, terms, correlation.matrix) {
#     return(sapply(terms, getCorrelation, string2 = term, correlation.matrix = correlation.matrix))
# }

# getCorrelationScoresForSynonyms = function(terms, correlation.matrix) {
#     return(list(terms, getCorrelationScoresForTerms(terms[1], terms, correlation.matrix)))
# }


# findSynonymsInTDM = function(term.matrix, correlation.matrix, max.cor = 0) {
#     terms = colnames(term.matrix)
#     all.syns = lapply(terms, getAllSynonyms, terms = terms)
#     syn.scores = lapply(all.syns, getCorrelationScoresForSynonyms, correlation.matrix = correlation.matrix)

#     return(syn.scores)
# }
