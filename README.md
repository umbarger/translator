# The Translator

This program will translate from a foreign language (or your choice of language) to English. You will receive two files, a file that contains foreign text (written in Latin characters) and a glossary. The glossary will contain foreign words and their English counterpart. Your program must process the foreign text file and translate it word by word by looking up each word in the glossary to find the English equivalent. 

The translation must be interleaved. The original foreign text must be displayed on one line with the English translation displayed on the line underneath it. You must write the translation to a file. The words must be spaced so that there is no overlapping, i.e., if the foreign word is longer than the English equivalent, then it must be filled out with blanks and vice versa. 

The glossary will have the following format: 

-	Each line will represent a different foreign word or phrase (up to 3 words) followed by a comma, which is followed by the English translation. 

-	The English translation may itself be a phrase of up to 3 words. 

-	There may be more than one English word or phrase for a given foreign word. If this is true, then each different English word (or phrase) will be followed by a comma and the next English word (or phrase). 

-	When translating, always use the first English word by default. You should, however, allow the user to enter a number that indicates the order to choose words if possible. For example, if the user enters 3, 2, 1, then you should use the third English word, if there are 3 or more English translations, the second English word, if there are exactly 2 English translations, and the first word otherwise. 

-	When translating, if there is no equivalent English word, put dashes where the English word would normally go. 

An example glossary would be: 

	aenvih,because
	aeq,huh
	aeu,get,want,use,take
	aeuj,purple
	aeuq,stew
	aeuqheiq,get angry
	baenxlai aek,so much,so very, thanks