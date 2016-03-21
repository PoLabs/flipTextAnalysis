#library(flipTextAnalysis)
library(testthat)
context("Word bag creation")


# Only do spelling corrections
wb.spell = InitializeWordBag(ftaFavoriteThings, operations = c("spelling"))
transforms.spell = cbind(wb.spell$tokens, wb.spell$replace.tokens)
counts.spell = cbind(wb.spell$counts, wb.spell$replace.counts)
testthat::expect_equal_to_reference(transforms.spell, "transforms-fav-wb-spell.rds")
testthat::expect_equal_to_reference(counts.spell, "counts-fav-wb-spell.rds")

# Do both spelling and stemming
wb.spell.stem = InitializeWordBag(ftaFavoriteThings)
transforms.spell.stem = cbind(wb.spell.stem$tokens, wb.spell.stem$replace.tokens)
counts.spell.stem = cbind(wb.spell.stem$counts, wb.spell.stem$replace.counts)
testthat::expect_equal_to_reference(transforms.spell.stem, "transforms-fav-wb-spell-stem.rds")
testthat::expect_equal_to_reference(counts.spell.stem, "counts-fav-wb-spell-stem.rds")

# Add some manual replacements
reps = c("sprang", "spring", "kattens", "kittens", "mattens", "mittens")
my.replacements = matrix(reps, ncol = 2, byrow = TRUE)
wb.spell.stem.manual = InitializeWordBag(ftaFavoriteThings, operations = c("spelling", "replacement", "stemming"), manual.replacements = my.replacements)
transforms.spell.stem.manual = cbind(wb.spell.stem.manual$tokens, wb.spell.stem.manual$replace.tokens)
counts.spell.stem.manual = cbind(wb.spell.stem.manual$counts, wb.spell.stem.manual$replace.counts)
testthat::expect_equal_to_reference(transforms.spell.stem.manual, "transforms-fav-wb-spell-stem-manual.rds")
testthat::expect_equal_to_reference(counts.spell.stem.manual, "counts-fav-wb-spell-stem-manual.rds")

# Printing
expect_that(print.wordBag(wb.spell), not(throws_error()))
