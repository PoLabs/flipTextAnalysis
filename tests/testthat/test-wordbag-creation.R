context("Word bag creation")

test_that("Creating word bags with different operations", {
    # Only do spelling corrections
    wb.spell <- InitializeWordBag(ftaFavoriteThings, operations = c("spelling"))
    transforms.spell <- cbind(wb.spell$tokens, wb.spell$replace.tokens)
    counts.spell <- cbind(wb.spell$counts, wb.spell$replace.counts)
    expect_equal_to_reference(transforms.spell, "transforms-fav-wb-spell.rds")
    expect_equal_to_reference(counts.spell, "counts-fav-wb-spell.rds")
    expect_true(min(wb.spell$final.counts) >= wb.spell$min.frequency)

    # Do both spelling and stemming
    wb.spell.stem <- InitializeWordBag(ftaFavoriteThings)
    transforms.spell.stem <- cbind(wb.spell.stem$tokens, wb.spell.stem$replace.tokens)
    counts.spell.stem <- cbind(wb.spell.stem$counts, wb.spell.stem$replace.counts)
    expect_equal_to_reference(transforms.spell.stem, "transforms-fav-wb-spell-stem.rds")
    expect_equal_to_reference(counts.spell.stem, "counts-fav-wb-spell-stem.rds")
    expect_true(min(wb.spell.stem$final.counts) >= wb.spell.stem$min.frequency)

    # Add some manual replacements
    reps = c("sprang", "spring", "kattens", "kittens", "mattens", "mittens")
    my.replacements <- matrix(reps, ncol = 2, byrow = TRUE)
    wb.spell.stem.manual <- InitializeWordBag(ftaFavoriteThings, operations = c("spelling", "replacement", "stemming"), manual.replacements = my.replacements)
    transforms.spell.stem.manual <- cbind(wb.spell.stem.manual$tokens, wb.spell.stem.manual$replace.tokens)
    counts.spell.stem.manual <- cbind(wb.spell.stem.manual$counts, wb.spell.stem.manual$replace.counts)
    expect_equal_to_reference(transforms.spell.stem.manual, "transforms-fav-wb-spell-stem-manual.rds")
    expect_equal_to_reference(counts.spell.stem.manual, "counts-fav-wb-spell-stem-manual.rds")
    expect_true(min(wb.spell.stem.manual$final.counts) >= wb.spell.stem.manual$min.frequency)

    # Replacement phrases
    reps = c("whiskers on kittens", "hair on dogs", "blue satin", "red wool")
    my.replacements <- matrix(reps, ncol = 2, byrow = TRUE)
    wb.spell.stem.phrases <- InitializeWordBag(ftaFavoriteThings, operations = c("spelling", "replacement", "stemming"), manual.replacements = my.replacements)
    transforms.spell.stem.phrases <- cbind(wb.spell.stem.phrases$tokens, wb.spell.stem.phrases$replace.tokens)
    counts.spell.stem.phrases <- cbind(wb.spell.stem.phrases$counts, wb.spell.stem.phrases$replace.counts)
    expect_equal_to_reference(transforms.spell.stem.phrases, "transforms-fav-wb-spell-stem-phrases.rds")
    expect_equal_to_reference(counts.spell.stem.phrases, "counts-fav-wb-spell-stem-phrases.rds")
    expect_true(min(wb.spell.stem.phrases$final.counts) >= wb.spell.stem.phrases$min.frequency)

    # Phrase stopwords
    my.stops <- c("kittens", "favorite things", "schnitzel with noodles")
    wb.stop.phrases <- InitializeWordBag(ftaFavoriteThings, operations = c("spelling", "stemming"), stoplist = my.stops)
    transforms.stop.phrases <- cbind(wb.stop.phrases$tokens, wb.stop.phrases$replace.tokens)
    counts.stop.phrases <- cbind(wb.stop.phrases$counts, wb.stop.phrases$replace.counts)
    expect_equal_to_reference(transforms.stop.phrases, "transforms-fav-wb-stop-phrases.rds")
    expect_equal_to_reference(counts.stop.phrases, "counts-fav-wb-stop-phrases.rds")
    expect_true(min(wb.stop.phrases$final.counts) >= wb.stop.phrases$min.frequency)

    # Spelling, higher min frequency
    wb.spell.min <- InitializeWordBag(ftaFavoriteThings, operations = c("spelling"), min.frequency = 3)
    expect_true(min(wb.spell.min$final.counts) >= wb.spell.min$min.frequency)

    # Printing
    expect_error(print.wordBag(wb.spell), NA)
})

test_that("Handling of subsets (filters) and blank responses", {
    reps = c("sprang", "spring", "kattens", "kittens", "mattens", "mittens")
    my.replacements = matrix(reps, ncol = 2, byrow = TRUE)
    test.data.blanks <- c(ftaFavoriteThings, rep("", 9), ftaFavoriteThings, rep("", 9))
    wb.blanks <- InitializeWordBag(test.data.blanks,
                                   operations = c("spelling", "replacement", "stemming"),
                                   manual.replacements = my.replacements)

    expect_equal(wb.blanks$sample.description, "n = 102 cases used to process the text of a total of 120; 18 cases are blank before transformation; 18 cases are blank after transformation.")
    expect_error(print.wordBag(wb.blanks), NA)

    .find.favorite <- function(str) {
        return(length(grep("favorite", str, ignore.case = TRUE)) == 0 && length(grep("favourite", str, ignore.case = TRUE)) == 0 )
    }
    my.test.subset <- unname(sapply(test.data.blanks, .find.favorite))
    wb.blanks.subset <- InitializeWordBag(test.data.blanks,
                                          operations = c("spelling", "replacement", "stemming"),
                                          manual.replacements = my.replacements,
                                          subset = my.test.subset)
    expect_equal(wb.blanks.subset$sample.description, "n = 78 cases used to process the text of a total of 96 (my.test.subset); 18 cases are blank before transformation; 18 cases are blank after transformation.")
    expect_equal(length(which(wb.blanks.subset$final.tokens == "favourite")), 0)
    expect_error(print.wordBag(wb.blanks.subset), NA)

    my.test.subset.named <- my.test.subset
    attr(my.test.subset.named, "name") <- "Doesn't mention 'favourite'"
    wb.blanks.subset.named <- InitializeWordBag(test.data.blanks,
                                                operations = c("spelling", "replacement", "stemming"),
                                                manual.replacements = my.replacements,
                                                subset = my.test.subset.named)
    expect_equal(wb.blanks.subset.named$sample.description, "n = 78 cases used to process the text of a total of 96 (Doesn't mention 'favourite'); 18 cases are blank before transformation; 18 cases are blank after transformation.")
    expect_equal(length(which(wb.blanks.subset.named$final.tokens == "favourite")), 0)
    expect_error(print.wordBag(wb.blanks.subset.named), NA)

    my.test.subset <- unname(sapply(ftaFavoriteThings, .find.favorite))
    wb.subset <- InitializeWordBag(ftaFavoriteThings,
                                         operations = c("spelling", "replacement", "stemming"),
                                         manual.replacements = my.replacements,
                                         subset = my.test.subset)

    expect_equal(wb.subset$sample.description, "n = 39 cases used to process the text (my.test.subset)")
    expect_equal(length(which(wb.subset$final.tokens == "favourite")), 0)
    expect_error(print.wordBag(wb.subset), NA)


    wb.no.subset <- InitializeWordBag(test.data.blanks,
                                   operations = c("spelling", "replacement", "stemming"),
                                   manual.replacements = my.replacements,
                                   subset = TRUE)
    expect_equal(wb.no.subset$sample.description, "n = 102 cases used to process the text of a total of 120; 18 cases are blank before transformation; 18 cases are blank after transformation.")

    wb.stopwords <- InitializeWordBag(test.data.blanks,
                                      operations = c("spelling", "stemming"),
                                      stoplist = c("kittens", "things", "favourite", "wings", "moon", "springs", "roses", "raindrops", "mittens", "whiskers", "on", "and"),
                                      subset = TRUE)
    expect_equal(wb.stopwords$sample.description, "n = 102 cases used to process the text of a total of 120; 18 cases are blank before transformation; 22 cases are blank after transformation.")

})
