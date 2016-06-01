# Unit tests for all functions
library(flipTextAnalysis)
library(testthat)
library(flipU)
context("Unit Tests")

# Phrase input and replacement

test.phrase.input.string = "easy to use, user friendly, easy to learn"
test.phrases <- convertCommaSeparatedStringToVector(test.phrase.input.string)
test.string <- "this software is very easy to use and also quite easy to learn but it is not user friendly"
new.string <- replacePhrasesInText(test.string, test.phrases)
new.string.readable <- makeWordBagTextReadable(new.string)

expect_equal(new.string, "this software is very easy+to+use and also quite easy+to+learn but it is not user+friendly")
expect_equal(new.string.readable, test.string)


# Tokenizing

text <- ftaFavoriteThings
text <- tolower(text)
tokenized <- ftaTokenize(text)
trigram.tokenized <- nGramTokenize(tokenized, 3)
tokens.counts <- countUniqueTokens(tokenized)
tokens <- tokens.counts$tokens
counts <- tokens.counts$counts
test.mapped.tokens <- tokens
test.mapped.tokens[1] <- "TEST"
updated.tokens.counts <- getUpdatedCounts(tokens, counts, test.mapped.tokens)
mapped.tokenized <- mapTokenizedText(tokenized, tokens, test.mapped.tokens)

test.token.list = list(tokenized, trigram.tokenized, tokens.counts, updated.tokens.counts, mapped.tokenized)
expect_equal_to_reference(test.token.list, "tokenization-test.rds")

# Handle newlines in tokenization
test.newline.string <- "The look and layout.\r\nThat it uses an SPSS data file.\r\nThat statistics a easily accessible.\r\nQ reader"
expect_equal(ftaTokenize(test.newline.string), list(c("the", "look", "and", "layout", "that", "it", "uses",
                                                      "an", "spss", "data", "file", "that", "statistics", "a", "easily",
                                                      "accessible", "q", "reader")))

# Stopwords
test.stop.words <- findStopWords(tokens)
expect_equal_to_reference(test.stop.words, "stopwords-test.rds")

#Stemming

test.stem.names <- GetStemNames(tokens, counts)
expect_equal_to_reference(test.stem.names, "stem-name-test.rds")

# Spelling

test.spell.errors <- FindSpellingErrors(tokens)
test.corrections <- GetCorrections(tokens, counts, test.spell.errors, test.stop.words)
expect_equal_to_reference(test.corrections, "spell-corrections-test.rds")


# Helper functions for Q/DisplayR user interface

test.merge.words.string <- "ease:easy, easily:easy, sig:significance"
test.replacement.matrix <- interpretMergeWordsString(test.merge.words.string)

expect_equal_to_reference(test.replacement.matrix, "test-replacement-matrix.rds")

test.operations <- generateOperations(do.spell = TRUE, do.stem = TRUE, replacement.matrix = test.replacement.matrix)
expect_equal(test.operations, c("spelling","replacement","stemming"))

test.options <- getTextAnalysisOptions(phrases = test.phrase.input.string,
                                       extra.stopwords.text = c("hello"),
                                       replacements.text = test.merge.words.string,
                                       do.stem = TRUE,
                                       do.spell = TRUE)

expect_equal_to_reference(test.options, "test-options.rds")

expect_equal(makeWordBagTextReadable("1+1 is 2. This+is+a+phrase."), "1 1 is 2. This is a phrase.")

test.table.for.formatting <- as.data.frame(cbind(tokens.counts$tokens, tokens.counts$counts, test.mapped.tokens, updated.tokens.counts))


