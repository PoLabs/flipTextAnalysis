library(flipTextAnalysis)
context("Word bag creation")

# Default options - OLD
wb = InitializeWordBag(ftaFavoriteThings)
all.counts = cbind(wb$counts, wb$corrected.counts, wb$stemmed.counts)
rownames(all.counts) = wb$tokens
testthat::expect_equal_to_reference(all.counts, "fav-wb.rds")

# Only do spelling corrections
wb.spell = InitializeWordBag(ftaFavoriteThings, do.stemming = FALSE)
transforms.spell = cbind(wb.spell$tokens, wb.spell$final.tokens)
counts.spell = cbind(wb.spell$counts, wb.spell$final.counts)
testthat::expect_equal_to_reference(transforms.spell, "transforms-fav-wb-spell.rds")
testthat::expect_equal_to_reference(counts.spell, "counts-fav-wb-spell.rds")

# Do both spelling and stemming
wb.spell.stem = InitializeWordBag(ftaFavoriteThings)
transforms.spell.stem = cbind(wb.spell.stem$tokens, wb.spell.stem$final.tokens)
counts.spell.stem = cbind(wb.spell.stem$counts, wb.spell.stem$final.counts)
testthat::expect_equal_to_reference(transforms.spell.stem, "transforms-fav-wb-spell-stem.rds")
testthat::expect_equal_to_reference(counts.spell.stem, "counts-fav-wb-spell-stem.rds")

# Add some manual replacements
reps = c("sprang", "spring", "kattens", "kittens", "mattens", "mittens")
my.replacements = matrix(reps, ncol = 2, byrow = TRUE)
wb.spell.stem.manual = InitializeWordBag(ftaFavoriteThings, manual.replacements = my.replacements)
transforms.spell.stem.manual = cbind(wb.spell.stem.manual$tokens, wb.spell.stem.manual$final.tokens)
counts.spell.stem.manual = cbind(wb.spell.stem.manual$counts, wb.spell.stem.manual$final.counts)
testthat::expect_equal_to_reference(transforms.spell.stem.manual, "transforms-fav-wb-spell-stem-manual.rds")
testthat::expect_equal_to_reference(counts.spell.stem.manual, "counts-fav-wb-spell-stem-manual.rds")
