cleanTweetText = function(string, remove.username = TRUE, remove.URLS = TRUE, remove.hashtags = FALSE, remove.ats = FALSE, remove.RT.tags = TRUE, remove.digits = TRUE) {
  require(stringr)
  if (remove.URLS) {
    string = gsub("https?://\\S+", "", string)
  }

  if (remove.username) {
    string = gsub("^\\w+: ", "", string)
  }


  if (remove.hashtags) {
    string = gsub("#\\S+", "", string)
  }

  if (remove.ats) {
    string = gsub("@\\S+", "", string)
  }

  if (remove.RT.tags) {
    string = gsub("RT \\S+:", "", string)
    string = gsub("ReTw \\S+:", "", string)
  }

  string = gsub("&amp;", "", string)
  string = gsub("[[:punct:][:cntrl:]]", "", string)
  string = gsub("[^[:print:]]", "", string)

  #     if (remove.digits) {
  #         string = gsub("\\d", "", string)
  #     }

  string = tolower(string)
  string = str_trim(string)
  string = gsub("\\s+", " ", string)

  return(string)
}

extractTextFromTweet = function(tweet) {
  return(tweet$text)
}
