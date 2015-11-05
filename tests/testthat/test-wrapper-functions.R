library(flipTextAnalysis)
context("Wrapper functions")

reps = c("sprang", "spring", "ponees", "ponies", "whiskers", "moustache")
test.replacements = matrix(reps, ncol = 2, byrow = TRUE)

# Term Matrix
test.tm = AsTermMatrix(ftaFavoriteThings, operations = c("spelling", "replacement", "stemming"), min.frequency = 2, manual.replacements = test.replacements)
testthat::expect_equal_to_reference(test.tm, "term-matrix-fav.rds")


# Sentiment Scores
test.sentiment.matrix = AsSentimentMatrix(ftaFavoriteThings)
testthat::expect_equal_to_reference(test.sentiment.matrix, "sent-matrix-fav.rds")


#Most Frequent Words
test.freq.words = MostFrequentWords(ftaFavoriteThings, operations = c("spelling", "replacement", "stemming"), manual.replacements = test.replacements, min.frequency = 2)
testthat::expect_equal_to_reference(test.freq.words, "freq-words-fav.rds")
