#Objective: ID the col and row numb to pull word and determein if identical to target word
 
WordRowMatch = function(Word, InputDF){

	#Construct this df to fill with contrast values to then evaluate
	Contrastdf = data.frame("RowMatch" = matrix(0, 1, nrow = nrow(STWdf)))
	
	#Extract all match of word
	Output= data.frame("Loci" = grep(Word ,t(InputDF )), stringsAsFactors=FALSE)

		#Identify if word is present in all of String to Word df, STWdf
		if(nrow(Output)>0){

		Output$Mod = Output$Loci %% ncol(InputDF )
		Output$Col = Output$Mod 
		Output$Col[Output$Col == 0] <- ncol(InputDF)
		Output$Row = ((Output$Loci - Output$Col)/ncol(InputDF ))+1

		for(i in 1:nrow(Output)){Output$Letter[i] = InputDF[Output$Row[i] , Output$Col[i]]}

		Output = cbind(Output, "Match" = (Word == Output$Letter))
		Row_loci = list(unique(subset(Output, Match == TRUE, select= Row))[[1]])[[1]]

		#Populate Contrastdf with conditional common, absent and new Word values
		Contrastdf$RowMatch[Row_loci] = 1

		} else{}

	#Output Contrastdf to OutputLoci for further analysis
	OutputPath = paste(OutputLoci , "/", Word, ".csv", sep = "", collapse = NULL)
	write.csv(Contrastdf, file = OutputPath , na="", row.names=FALSE)

}

####################################
#Objective: ID the col and row numb to pull word and determein if identical to target word
 
RowMatch = function(Word, InputDF){

	Output= data.frame("Loci" = grep(Word ,t(InputDF )), stringsAsFactors=FALSE)

	#Construct this df to fill with contrast values to then evaluate
	Contrastdf = data.frame(Word = matrix(0, 1, nrow = nrow(InputDF)))

	if(nrow(Output)>0){
	
	Output$Mod = Output$Loci %% ncol(InputDF )
	Output$Col = Output$Mod 
	Output$Col[Output$Col == 0] <- ncol(InputDF)
	Output$Row = ((Output$Loci - Output$Col)/ncol(InputDF ))+1

	for(i in 1:nrow(Output)){Output$Letter[i] = InputDF[Output$Row[i] , Output$Col[i]]}

	Output = cbind(Output, "Match" = (Word == Output$Letter))
	Row_loci = list(unique(subset(Output, Match == TRUE, select= Row))[[1]])[[1]]

	} else {}

	Contrastdf$Word[Row_loci] = 1
	names(Contrastdf) = Word
	return(Contrastdf)
}

####################################
#Objective: ID the col and row numb to pull word and determein if identical to target word
 
RowMatchCount = function(Word, InputDF){

	Output= data.frame("Loci" = grep(Word ,t(InputDF )), stringsAsFactors=FALSE)

	#Construct this df to fill with contrast values to then evaluate
	Contrastdf = data.frame(Word = matrix(0, 1, nrow = nrow(InputDF)))

	if(nrow(Output)>0){
	
	Output$Mod = Output$Loci %% ncol(InputDF )
	Output$Col = Output$Mod 
	Output$Col[Output$Col == 0] <- ncol(InputDF)
	Output$Row = ((Output$Loci - Output$Col)/ncol(InputDF ))+1

	for(i in 1:nrow(Output)){Output$Letter[i] = InputDF[Output$Row[i] , Output$Col[i]]}

	Output = cbind(Output, "Match" = (Word == Output$Letter))
	Row_loci = as.numeric(as.character(list(subset(Output, Match == TRUE, select= Row)[[1]])[[1]]))
	WordFreq = data.frame(table(Row_loci))
	
	} else {}

	Contrastdf$Word[as.numeric(as.character(WordFreq$Row_loci)) ] <- as.numeric(as.character(WordFreq$Freq))
	names(Contrastdf) = Word
	return(Contrastdf)
}



	#for(i in 1:nrow(WordFreq)){
	#row = as.numeric(as.character(WordFreq$Row_loci[i]))
	#count = as.numeric(as.character(WordFreq$Freq[i]))
	#Contrastdf$Word[row ] <- count 
	#}

