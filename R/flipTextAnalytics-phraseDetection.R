
# Supply
replacePhrasesInText = function(text, phrases)
{
	tagged.phrases = convertPhrasesToTagged(phrases)
	for (j in 1L:length(phrases))
	{
		text = gsub(phrases[j], tagged.phrases[j], text)
	}
	return(text)	
}

convertPhrasesToTagged = function(phrases) 
{
    split.phrases = Tokenize(phrases)
    tagged.phrases = sapply(split.phrases, paste, collapse = "+")
    return(tagged.phrases)
}