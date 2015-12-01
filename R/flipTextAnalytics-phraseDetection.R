# This function adds a delimiter to any phrases found in the text.
# In this framework when we want to treat a sequence of words as a
# single unit we join them with "+" so that they don't get split
# by the tokenizer, which uses whitespace. Given a character vector
# of phases, this function iterates through the character vector
# of text and adds the delimiter.
replacePhrasesInText <- function(text, phrases)
{
	.convertPhrasesToTagged <- function(phrases) 
	{
	    split.phrases <- ftaTokenize(phrases)
	    tagged.phrases <- sapply(split.phrases, paste, collapse = "+")
	    return(tagged.phrases)
	}

	tagged.phrases <- .convertPhrasesToTagged(phrases)
	for (j in 1L:length(phrases))
	{
		text <- gsub(phrases[j], tagged.phrases[j], text)
	}
	return(text)	
}

	