#######################################
#
# Program to analyze college level data
# to create a simple college ranking
# See shorttails.io
#
#######################################

library(plyr)
library(dplyr)
library(ggplot2)
library(maps)
library(reshape2)
library(gridExtra)
library(gplots)
library(scales)
library(xtable)

##### Part 1: Load in the data

scorecardrawdata2011 = read.csv("~/Dropbox/Short Tails/College Rankings/CollegeScorecard_Raw_Data/MERGED2011_PP.csv",header=TRUE,comment.char="",quote="",stringsAsFactors=FALSE)
scorecardrawdata2013 = read.csv("~/Dropbox/Short Tails/College Rankings/CollegeScorecard_Raw_Data/MERGED2013_PP.csv",header=TRUE,comment.char="",quote="",stringsAsFactors=FALSE)

# Extract variables of interest (latitude and longitude only available in 2013 data)
scorecardpulldata2011 = scorecardrawdata3[,c("UNITID","INSTNM","CITY","STABBR","SAT_AVG","CURROPER","COSTT4_A","GRAD_DEBT_MDN","md_earn_wne_p6","UGDS","CONTROL","LO_INC_DEBT_MDN","ADM_RATE_ALL")]
scorecardpulldata2013 = scorecardrawdata[,c("UNITID","LATITUDE","LONGITUDE")]
scorecardpullmerge = merge(scorecardpulldata2013,scorecardpulldata2011,by="UNITID")

##### Part 2: Format and clean up the data for analysis

converttonum = c(1,2,3,7,8,9,10,11,12,13,14,15)
scorecardpullmergec = scorecardpullmerge
scorecardpullmergec[,converttonum] = sapply(scorecardpullmerge[,converttonum],as.numeric)
scorecardpullmergecc = scorecardpullmergec[which(scorecardpullmergec$CURROPER==1 & scorecardpullmergec$UGDS>=100),] # Only keep institutions currently operating and those with at least 100 students
checknaforrem = with(scorecardpullmergecc,which(is.na(GRAD_DEBT_MDN) | is.na(md_earn_wne_p6)))
scorecarddata = scorecardpullmergecc[-checknaforrem,]

##### Part 3: Analysis of college student debt and earnings

# Calculate ratio of earnings to debt
scorecarddatap = data.frame(scorecarddata,edratio=(scorecarddata$md_earn_wne_p6/scorecarddata$GRAD_DEBT_MDN))

# Scatterplot of earnings versus debt
earndebtmed = data.frame(earnmedian=median(scorecarddatap$md_earn_wne_p6),debtmedian=median(scorecarddatap$GRAD_DEBT_MDN))
print(earndebtmed$earnmedian); print(earndebtmed$debtmedian)
png("./plots/rankingscatter.png",width=5,height=5,units="in",res=300)
ggplot(data=scorecarddatap) + geom_point(aes(x=md_earn_wne_p6,y=GRAD_DEBT_MDN),alpha=.8,size=2) + theme_bw(base_size=15) + scale_x_continuous(breaks=seq(0,120000,40000),limits=c(0,130000)) + scale_y_continuous(breaks=seq(0,50000,10000),limits=c(0,50000)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank()) + geom_abline(intercept=0,slope=1,col="red") + geom_point(data=earndebtmed,aes(x=earnmedian,y=debtmedian),col="red",size=3,pch=15) + ylab("") + xlab("")
#ggplot(data=scorecarddatap) + geom_point(aes(x=md_earn_wne_p6,y=GRAD_DEBT_MDN),alpha=.8,size=2) + theme_bw(base_size=15) + scale_x_continuous(breaks=seq(0,120000,40000),limits=c(0,130000)) + scale_y_continuous(breaks=seq(0,50000,10000),limits=c(0,50000)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank()) + geom_abline(intercept=0,slope=1,col="red") + geom_point(data=earndebtmed,aes(x=earnmedian,y=debtmedian),col="red",size=3,pch=15) + ylab("") + xlab("") + geom_text(aes(x=md_earn_wne_p6,y=GRAD_DEBT_MDN,label=ifelse(GRAD_DEBT_MDN>42000 | md_earn_wne_p6>80000,as.character(INSTNM),"")),vjust=0)
dev.off()

# Print correlation between debt and earnings
print(cor(scorecarddatap$md_earn_wne_p6,scorecarddatap$GRAD_DEBT_MDN,method="spearman"))

# Plot map of US with college locations colored by earnings to debt ratio and size of dot undergraduate population
scorecarddatapmap = scorecarddatap[-which(scorecarddatap$LATITUDE < 25 | scorecarddatap$LONGITUDE < (-130)),]
all_states = map_data("state")
rankingmapbase = ggplot() + geom_polygon(data=all_states, aes(x=long, y=lat, group=group),colour="white", fill="grey10") + theme_bw(base_size=25) + theme(axis.line = element_line(colour = "black"),panel.grid = element_blank(),panel.border = element_blank())
rankingmap = rankingmapbase + geom_point(data=scorecarddatapmap, aes(x=LONGITUDE, y=LATITUDE, size=log10(UGDS), color=log10(edratio), alpha=(edratio^2))) + scale_size(name="Total Enrollment") + scale_color_continuous(name="Earnings to Debt Ratio",low="light grey",high="red")
png("./plots/rankingscatter.png",width=7,height=4,units="in",res=300)
rankingmap
dev.off()

# Create table of ratio rankings
scorecarddatapforwrite = scorecarddatap[,c("INSTNM","CITY","STABBR","UGDS","edratio","md_earn_wne_p6","GRAD_DEBT_MDN","SAT_AVG","ADM_RATE_ALL")]
colnames(scorecarddatapforwrite) = c("Name","City","State","Enrollment","E/D Ratio","Earnings","Debt","SAT","Admission %")
scorecarddatapforwriteo = scorecarddatapforwrite[order(scorecarddatapforwrite[,5],decreasing=TRUE),]
write.table(scorecarddatapforwriteo,file="earningsvsdebtratio.txt",quote=FALSE,row.names=FALSE,col.names=TRUE,sep="\t")

# Create HTML table of top and bottom 10 colleges
forwritehtml = scorecarddatap[,c("INSTNM","CITY","STABBR","UGDS","edratio","md_earn_wne_p6","GRAD_DEBT_MDN")]
colnames(forwritehtml) = c("Name","City","State","Enrollment","E/D Ratio","Earnings","Debt")
forwritehtmlo = forwritehtml[order(forwritehtml$"E/D Ratio",decreasing=TRUE),]
forwritehtmlob = forwritehtml[order(forwritehtml$"E/D Ratio",decreasing=FALSE),]
forwritehtmlotop = head(forwritehtmlo,10)
forwritehtmlobottom = head(forwritehtmlob,10)
forwritehtmlotophtml = forwritehtmlotop[,-1]
rownames(forwritehtmlotophtml) = forwritehtmlotop[,1]
forwritehtmlobottomhtml = forwritehtmlobottom[,-1]
rownames(forwritehtmlobottomhtml) = forwritehtmlobottom[,1]
sink("topearningdebtratiohtmltable.html")
print(xtable(forwritehtmlotophtml,display=c("s","s","s","d","f","d","d")),type="html")
sink()
sink("bottomearningdebtratiohtmltable.html")
print(xtable(forwritehtmlobottomhtml,display=c("s","s","s","d","f","d","d")),type="html")
sink()

# Scatterplot of earnings versus debt for students whose families earn $0-30,000
scorecarddataplo = data.frame(scorecarddata,edratio=(scorecarddata$md_earn_wne_p6/scorecarddata$LO_INC_DEBT_MDN))
# Scatterplot of earnings versus debt
earndebtmedlo = data.frame(earnmedian=median(scorecarddataplo$md_earn_wne_p6),debtmedian=median(scorecarddataplo$LO_INC_DEBT_MDN,na.rm=TRUE))
print(earndebtmedlo$earnmedian); print(earndebtmedlo$debtmedian)
png("./plots/rankingscatterlow.png",width=5,height=5,units="in",res=300)
ggplot(data=scorecarddataplo) + geom_point(aes(x=md_earn_wne_p6,y=LO_INC_DEBT_MDN),alpha=.8,size=2) + theme_bw(base_size=15) + scale_x_continuous(breaks=seq(0,120000,40000),limits=c(0,130000)) + scale_y_continuous(breaks=seq(0,50000,10000),limits=c(0,50000)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank()) + geom_abline(intercept=0,slope=1,col="red") + geom_point(data=earndebtmedlo,aes(x=earnmedian,y=debtmedian),col="red",size=3,pch=15) + ylab("") + xlab("")
#ggplot(data=scorecarddataplo) + geom_point(aes(x=md_earn_wne_p6,y=LO_INC_DEBT_MDN),alpha=.8,size=2) + theme_bw(base_size=15) + scale_x_continuous(breaks=seq(0,120000,40000),limits=c(0,130000)) + scale_y_continuous(breaks=seq(0,50000,10000),limits=c(0,50000)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank()) + geom_abline(intercept=0,slope=1,col="red") + geom_point(data=earndebtmedlo,aes(x=earnmedian,y=debtmedian),col="red",size=3,pch=15) + ylab("") + xlab("") + geom_text(aes(x=md_earn_wne_p6,y=LO_INC_DEBT_MDN,label=ifelse(LO_INC_DEBT_MDN>35000 | md_earn_wne_p6>80000,as.character(INSTNM),"")),vjust=0)
dev.off()

# Create table of ratio rankings for low income families
print(cor(scorecarddatap$edratio,scorecarddataplo$edratio,method="spearman",use="complete.obs"))
scorecarddatapforwritelo = scorecarddataplo[,c("INSTNM","CITY","STABBR","UGDS","edratio","md_earn_wne_p6","LO_INC_DEBT_MDN","SAT_AVG","ADM_RATE_ALL")]
colnames(scorecarddatapforwritelo) = c("Name","City","State","Enrollment","E/D Ratio","Earnings","Low Income Debt","SAT","Admission %")
scorecarddatapforwriteloo = scorecarddatapforwritelo[order(scorecarddatapforwritelo[,5],decreasing=TRUE),]
write.table(scorecarddatapforwriteloo,file="earningsvsdebtratiolowincome.txt",quote=FALSE,row.names=FALSE,col.names=TRUE,sep="\t")

# Create HTML table of top 10 colleges for low income
forwritehtmllo = scorecarddataplo[,c("INSTNM","CITY","STABBR","UGDS","edratio","md_earn_wne_p6","LO_INC_DEBT_MDN")]
colnames(forwritehtmllo) = c("Name","City","State","Enrollment","E/D Ratio","Earnings","Low Income Debt")
forwritehtmlloo = forwritehtmllo[order(forwritehtmllo$"E/D Ratio",decreasing=TRUE),]
forwritehtmllootop = head(forwritehtmlloo,10)
forwritehtmllootophtml = forwritehtmllootop[,-1]
rownames(forwritehtmllootophtml) = forwritehtmllootop[,1]
sink("topearningdebtratiolowincomehtmltable.html")
print(xtable(forwritehtmllootophtml,display=c("s","s","s","d","f","d","d")),type="html")
sink()

##### Part 4: Analysis of college student debt and earnings compared to college selectivity

print(cor(scorecarddatap$edratio,scorecarddatap$SAT_AVG,use="complete.obs"))
print(cor(scorecarddatap$edratio,scorecarddatap$ADM_RATE_ALL,use="complete.obs"))
# Scatterplot of earnings to debt ratio vs sat score
png("./plots/ratiosatscatter.png",width=5,height=5,units="in",res=300)
ggplot(data=scorecarddatap) + geom_point(aes(x=SAT_AVG,y=edratio),alpha=.8,size=2) + theme_bw(base_size=15) + scale_y_continuous(breaks=seq(0,16,4),limits=c(0,16)) + scale_x_continuous(breaks=seq(600,1600,400),limits=c(600,1650)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank()) + ylab("") + xlab("") + geom_vline(xintercept=1300,col="red")
dev.off()
# Scatterplot of earnings to debt ratio vs admissions rate
png("./plots/ratioadmitscatter.png",width=5,height=5,units="in",res=300)
ggplot(data=scorecarddatap) + geom_point(aes(x=ADM_RATE_ALL,y=edratio),alpha=.8,size=2) + theme_bw(base_size=15) + scale_y_continuous(breaks=seq(0,16,4),limits=c(0,16)) + scale_x_continuous(breaks=seq(0,1,0.2),limits=c(0,1)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank()) + ylab("") + xlab("") + geom_vline(xintercept=0.2,col="red")
dev.off()

# Correlations between earnings and SAT or admission rate
print(cor(scorecarddatap$md_earn_wne_p6,scorecarddatap$SAT_AVG,use="complete.obs"))
print(cor(scorecarddatap$md_earn_wne_p6,scorecarddatap$ADM_RATE_ALL,use="complete.obs"))


