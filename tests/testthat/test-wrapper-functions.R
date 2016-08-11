library(flipTextAnalysis)
context("Wrapper functions")

reps = c("sprang", "spring", "ponees", "ponies", "whiskers", "moustache")
test.replacements = matrix(reps, ncol = 2, byrow = TRUE)

test_that("Term Matrix", {
    test.tm = AsTermMatrix(ftaFavoriteThings, operations = c("spelling", "replacement", "stemming"), min.frequency = 2, manual.replacements = test.replacements)
    expect_equal_to_reference(test.tm, "term-matrix-fav.rds")

    test.tm.from.text <- termMatrixFromText(ftaFavoriteThings)
    expect_equal_to_reference(test.tm.from.text, "term-matrix-from-text-fav.rds")

    test.tm.phrases <- AsTermMatrix(InitializeWordBag(ftaFavoriteThings, phrases = "favorite things"))
    expect_equal_to_reference(test.tm.phrases, "term-matrix-with-phrases-fav.rds")
})


test_that("Sentiment Scores", {
    test.sentiment.matrix = AsSentimentMatrix(ftaFavoriteThings)
    expect_equal_to_reference(test.sentiment.matrix, "sent-matrix-fav.rds")
})

test.wb = InitializeWordBag(ftaFavoriteThings, operations = c("spelling", "replacement", "stemming"), manual.replacements = test.replacements, min.frequency = 2)
test_that("Most Frequent Words", {
    test.freq.words = MostFrequentWords(test.wb)
    expect_equal_to_reference(test.freq.words, "freq-words-fav.rds")
})

test_that("Term matrix from cleaned Text", {
    test.matrix.from.cleaned = AsTermMatrix(test.wb$transformed.text, min.frequency = 4)
    expect_equal_to_reference(test.matrix.from.cleaned, "cleaned-matrix-fav.rds")
})

