library(flipTextAnalysis)
context("Word bag creation")

wb = InitializeWordBag(ftaFavoriteThings)
all.counts = cbind(wb$counts, wb$corrected.counts, wb$stemmed.counts)
rownames(all.counts) = wb$tokens
testthat::expect_equal_to_reference(all.counts, "fav-wb.rds")
