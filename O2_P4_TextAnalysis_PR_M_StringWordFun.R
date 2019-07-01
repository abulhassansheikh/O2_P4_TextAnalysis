FormatStringcolumn <- function(TextCol, ColName, WordColName, noDup1all0){

Textdf = data.frame(TextCol)
names(Textdf) = "Text"

#Clean Up Original Column into a reference DF
Textdf_dup =  if(noDup1all0 == 1){data.frame(Textdf[!duplicated(trimws(Textdf$Text) ),])}else{Textdf}
Textdf_blank = subset(Textdf_dup, !is.na(Textdf_dup$Text) & Textdf_dup$Text != "")
Textdf_sorted = Textdf_blank[order(as.character(Textdf_blank$Text)),]

Textdf_REF = data.frame(Textdf_sorted)
names(Textdf_REF) = "Text"


#Standardize Text in original Column
Textdf_char = as.character(Textdf_REF$Text)#Turn into character
Textdf_lower = tolower(Textdf_char)#lower case
Textdf_ac = gsub("A/C", "AC", Textdf_lower)
Textdf_punc = gsub("[[:punct:]]", " ",Textdf_ac)#To remove punctuations
Textdf_tripleSpace = gsub("   ", " ", Textdf_punc)
Textdf_doubleSpace = gsub("  ", " ", Textdf_tripleSpace)


#Final text df
Textdf_clean = data.frame(Textdf_doubleSpace)
names(Textdf_clean) = "Text"
if(nrow(Textdf_REF) != nrow(Textdf_clean)){message("Mismatch Error")}


#Find most number of words
check_length = as.matrix(as.character(Textdf_clean$Text))
length = sapply(strsplit(check_length, " "), length)
maxLength = max(length )

#message("Creating Word Breakdown File")
WordREF = data.frame()

#Create seperated word df
	for(i in 1:nrow(Textdf_clean)){

		String = as.character(Textdf_clean[i,1])
	
		text_2worddf = t(data.frame(strsplit(String, " ")))#Break words into individual columns
		wordwsTrim = trimws(text_2worddf)#Trim white space

		#Add dummy DUMB columns
		Addcol = maxLength - ncol(wordwsTrim)
		Dumbmt = matrix("DUMB", nrow=1, ncol= Addcol , byrow=TRUE)

		textbreakdown = cbind(as.character(Textdf_REF[i,1]),  wordwsTrim, Dumbmt)

		WordREF  = rbind(WordREF, textbreakdown)
	}


#Create DF col names
namestring  = ColName
for(w in 1:maxLength ){
namestring = c(namestring, paste(WordColName, w, sep="", collapse = NULL))
}

names(WordREF) = namestring
  
WordREF= data.frame(lapply(WordREF, as.character), stringsAsFactors=FALSE)
#message("Word Data Created")
#############
#Find out max number of elements per word column

MaxUnqWords = 0

for(m in 2:ncol(WordREF)){
	
	len = length(WordREF[!duplicated( WordREF[,m] ),][,m])
	MaxUnqWords = c(MaxUnqWords, len)	
}

MaxUnqWords = max(MaxUnqWords)

#############
#message("Analyzing Text Data")
library(plyr)

UnqWordsCount_Col = data.frame(matrix("DUMB", nrow=MaxUnqWords , ncol= 1, byrow=TRUE))
UnqWordsCount_all_raw = data.frame()

for(m in 2:ncol(WordREF)){
	
	ColData_raw = ddply(WordREF, .(WordREF[,m]), nrow)
	ColData_ord = ColData_raw[order(-ColData_raw[,2]),]
	ColData_lit = subset(ColData_ord, ColData_ord[,1] !="DUMB")
	
	#Add dummy DUMB columns
	Addcol = MaxUnqWords - nrow(ColData_lit)
	Dumbmt = data.frame(t(matrix("DUMB", nrow=2, ncol= Addcol , byrow=TRUE)))
	names(Dumbmt) = names(ColData_lit)

	ColData_ready = rbind(ColData_lit, Dumbmt)
	names(ColData_ready) = c(m, paste(m,"count", sep="", collpase=NULL))

	UnqWordsCount_Col = cbind(UnqWordsCount_Col, ColData_ready)
######
	UnqWordsCount_all_raw = rbind(UnqWordsCount_all_raw, data.frame(WordREF[,m]))

}

UnqWordsCount_Col = UnqWordsCount_Col[, -1]

UnqWordsCount_alldata = ddply(UnqWordsCount_all_raw, .(UnqWordsCount_all_raw[,1]), nrow)
UnqWordsCount_allord = UnqWordsCount_alldata[order(-UnqWordsCount_alldata[,2]),]
UnqWordsCount_all = subset(UnqWordsCount_allord, UnqWordsCount_allord[,1] !="DUMB")

#message("Done")
#message("[1] = String to Word (STWdf), [2] = All Word List (AWLdf), [3] = Word Data by Col, [4] = Col Names")
return(list(WordREF, UnqWordsCount_all, UnqWordsCount_Col, namestring))
}

#######################################
#############
#TROUBLESHOOT
#DCIptWordREF = FormatStringcolumn(TextCol = DCIpt_raw$PartType, ColName = "dciptdescr", WordColName = "PT")





