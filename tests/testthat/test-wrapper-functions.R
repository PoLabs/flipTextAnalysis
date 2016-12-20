library(flipTextAnalysis)
context("Wrapper functions")

reps <- c("sprang", "spring", "ponees", "ponies", "whiskers", "moustache")
test.replacements <- matrix(reps, ncol = 2, byrow = TRUE)



test_that("Term Matrix", {
    test.tm = AsTermMatrix(ftaFavoriteThings, operations = c("spelling", "replacement", "stemming"), min.frequency = 2, manual.replacements = test.replacements)
    expect_equal_to_reference(test.tm, "term-matrix-fav.rds")

    test.tm.from.text <- termMatrixFromText(ftaFavoriteThings)
    expect_equal_to_reference(test.tm.from.text, "term-matrix-from-text-fav.rds")

    test.tm.phrases <- AsTermMatrix(InitializeWordBag(ftaFavoriteThings, phrases = "favorite things"))
    expect_equal_to_reference(test.tm.phrases, "term-matrix-with-phrases-fav.rds")
})

test_that("Sentiment Tags", {
    trump.wb <- InitializeWordBag(trumpTweets$text, operations = '', min.frequency = 5)
    test.sentiment.tags = TagSentiment(trump.wb$final.tokens, check.simple.suffixes = TRUE)
    expect_equal_to_reference(test.sentiment.tags, "trump-sentiment-tags.rds")
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



test_that("Calculate NET Sentiment Scores", {
    my.test.subset <- rep(c(TRUE, TRUE, FALSE), 17)
    test.wb <- InitializeWordBag(ftaFavoriteThings, operations = c("spelling"), subset = my.test.subset)
    expect_equal(unname(SaveNetSentimentScores(test.wb)), c(0, 2, 0, 1, 1, 0, -1, 1, 0, 0, 0, 1, 0, 0, -1, 1, -1, 0, 2,
                              0, 1, 1, 0, -1, 1, 0, 0, 0, 1, 0, 0, -1, 1, -1, 0, 2, 0, 1, 0,
                              0, -1, 1, 0, 0, 0, 1, 0, -1, -1, 1, -1))
    expect_equal(unname(SaveNetSentimentScores(ftaFavoriteThings)), c(0, 2, 0, 1, 1, 0, -1, 1, 0, 0, 0, 1, 0, 0, -1, 1, -1, 0, 2,
                                                             0, 1, 1, 0, -1, 1, 0, 0, 0, 1, 0, 0, -1, 1, -1, 0, 2, 0, 1, 0,
                                                             0, -1, 1, 0, 0, 0, 1, 0, -1, -1, 1, -1))
    trump.wb <- InitializeWordBag(trumpTweets$text, operations = '', min.frequency = 5)
    test.trump.sentiment <- SaveNetSentimentScores(trump.wb, check.simple.suffixes = TRUE)
    expect_equal_to_reference(test.trump.sentiment, "trump-net-sentiment-scores.rds")

})

test_that("Save Tidied Text", {
    my.test.subset <- rep(c(TRUE, TRUE, FALSE), 17)
    test.wb <- InitializeWordBag(ftaFavoriteThings, operations = c("spelling"), subset = my.test.subset)
    test.tidy.text = SaveTidiedText(test.wb)
    expect_equal(test.tidy.text, c("raindrops roses whiskers kittens", "bright copper kettles warm woolen mittens",
                                   "brown paper packages tied up with strings", "few favorite",
                                   "cream colored ponies crisp apple streudels", "doorbells and sleigh bells and schnitzel with noodles",
                                   "wild geese fly moon wings", "few favorite", "girls in white dresses with blue satin sashes",
                                   "snowflakes stay nose eyelashes", "silver white winters melt springs",
                                   "these are a few of my favorite things", "dog bites", "bee stings",
                                   "when i'm feeling sad", "simply remember favorite", "dont feel bad",
                                   "raindrops on roses and whiskers on kittens", "bright copper kettles warm woolen mittens",
                                   "brown paper packages tied strings", "these are a few of my favorite things",
                                   "cream colored ponies crisp apple streudels", "doorbells sleigh bells schnitzel noodles",
                                   "wild geese that fly with the moon on their wings", "few favorite",
                                   "girls white dresses blue satin sashes", "snowflakes that stay on my nose and eyelashes",
                                   "silver white winters melt springs", "few favorite", "when the dog bites",
                                   "bee stings", "feeling sad", "i simply remember my favorite things",
                                   "dont feel bad", "raindraps noses whiskers kattens", "bright cawpper kattles and warm wolen mattens",
                                   "brun paper packages tied string", "few favourite", "creem coloured ponees and krisp apple streudels",
                                   "doorbells sleigh bells schnitzel noodles", "wild geese fly moon wing",
                                   "these are a few of my favorite things", "girl white dress blue satin sash",
                                   "snowflakes stay nose eyelash", "silver whit winters that melt into sprang",
                                   "few favorite", "dawg bights", "when the bee sting", "feeling sad",
                                   "simply remember favorite", "and then i don't feel so bad"))
})
