#######################################
#
# Program to download the 
# different global indicators
# from Gapminder
#
#######################################

require(RCurl)

##### Part 1: Load in the spreadsheet URLs

spreadsheeturls = read.table(file="spreadsheeturls.txt",stringsAsFactors=FALSE,quote="")
spreadsheeturlsclean = sapply(strsplit(spreadsheeturls[,1],split='"'),"[",2)
spreadsheeturlscleanu = unique(spreadsheeturlsclean)

##### Part 2: Download each spreadsheet

cururls = vector("character",length(spreadsheeturlscleanu))
for(i in 1:length(spreadsheeturlscleanu)) {
	print(i)
	cururl = getURL(paste("https://spreadsheets.google.com/pub?key=",spreadsheeturlscleanu[i],"&output=tsv",sep=""))
	cururls[i] = xpathSApply(htmlParse(cururl),"//a/@href")
}
cururlstrim = cururls[(nchar(cururls)>5)]
setwd("/Users/trevormartin/shorttails/indicatorcors/spreadsheets/")
# Cycles through each URL and downloads the spreadsheet as tab separated values
for(i in 1:length(cururlstrim)) {
	print(i)
	system(paste("curl ",cururlstrim[i],"> spreadsheet",i,sep=""))
}
