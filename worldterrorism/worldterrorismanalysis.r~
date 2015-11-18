#######################################
#
# Program to analyze global terrorism
# data to look at changes over time
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
library(RColorBrewer)
library(pheatmap)
library(xtable)

##### Part 1: Load in the data

rawdata = read.csv("~/Dropbox/Short Tails/World Terrorism/gtd_0615dist/globalterrorismdb_0615dist.csv")
rawdata$eventid = as.character(rawdata$eventid)

##### Part 2: Clean up the data

pullcolumns = c("eventid","iyear","imonth","iday","region_txt","attacktype1","targtype1","gname","nkill","nwound")
cleandatah1 = rawdata[,pullcolumns]
# Remove attacks that do not have date information
nodateinfo = (cleandatah1$imonth==0 | cleandatah1$iday==0)
cleandatah2 = cleandatah1[(!nodateinfo),]
killwoundtot = (cleandatah2$nkill+cleandatah2$nwound)
cleandatah3 = data.frame(cleandatah2,nkwtot = killwoundtot)
cleandata = cleandatah3

##### Part 3: Analyze the frequency and type of attacks over time

# Barplot of number of attacks each year
attacksbarplot = ggplot(data=cleandata,aes(x=as.factor(iyear))) + geom_bar(binwidth=1) + theme_bw(base_size=35) + xlab("") + ylab("") + theme(axis.text.x = element_blank(), axis.ticks=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank())
attacksbarplotnol = ggplot(data=cleandata,aes(x=as.factor(iyear))) + geom_bar(binwidth=1) + theme_bw(base_size=15) + xlab("") + ylab("") + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank())
# Stacked barplot of number killed/wounded each year
cleandatamy = melt(cleandata[,c("iyear","nkill","nwound")],id.var="iyear")
killwoundcolors = c("#F5DA0C","#F5333D"); names(killwoundcolors) = c("nwound","nkill")
killwoundbarplot = ggplot(data=cleandatamy,aes(x=as.factor(iyear),y=value,fill=variable)) + geom_bar(stat="identity") + theme_bw(base_size=50) + xlab("") + ylab("") + scale_fill_manual(values=killwoundcolors) + scale_y_reverse() + theme(axis.text.x = element_blank(), axis.ticks=element_blank())
killwoundbarplotnol = ggplot(data=cleandatamy,aes(x=as.factor(iyear),y=value,fill=variable)) + geom_bar(stat="identity") + theme_bw(base_size=15) + xlab("") + ylab("") + scale_fill_manual(values=killwoundcolors) + scale_y_reverse() + theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank())
# Heatmap of number of attacks each year/month
sumevents = aggregate(eventid~iyear+imonth+iday,cleandata,FUN=length)
sumeventso = sumevents[order(sumevents$imonth),]
sumeventsp = data.frame(sumeventso,monthday=paste(sumeventso$imonth,sumeventso$iday,sep="-"))
sumeventsp$monthday = factor(sumeventsp$monthday,levels=sumeventsp$monthday[!duplicated(sumeventsp$monthday)])
yearfach = unique(c(sumeventsp$iyear,1993))
sumeventsp$iyear = factor(sumeventsp$iyear,levels=yearfach[order(yearfach,decreasing=FALSE)])
numattacksheat = ggplot(data=sumeventsp,aes(x=iyear,y=monthday)) + geom_tile(aes(fill=eventid)) + scale_fill_gradient(low="white",high="darkblue") + theme_bw(base_size=50) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks=element_blank()) + xlab("") + ylab("") + scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0))
numattacksheatnol = ggplot(data=sumeventsp,aes(x=iyear,y=monthday)) + geom_tile(aes(fill=eventid)) + scale_fill_gradient(low="white",high="darkblue") + theme_bw(base_size=15) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks=element_blank(), legend.position="none") + xlab("") + ylab("") + scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0))
# numattacksheatnol = ggplot(data=sumeventsp,aes(x=iyear,y=monthday)) + geom_tile(aes(fill=eventid)) + scale_fill_gradient(low="white",high="darkblue") + theme_bw(base_size=100) + theme(axis.text.x = element_text(angle=(-90),hjust=0),axis.text.y = element_blank(), axis.ticks=element_blank(), legend.position="none") + xlab("") + ylab("") + scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0))
# Combine event frequency plots into one
png("./plots/attacksovertimewithhist.png",width=4000,height=3000)
grid.arrange(attacksbarplotnol,numattacksheatnol,killwoundbarplotnol,ncol=1,nrow=3,heights=c(1,2,1))
dev.off()
png("./plots/attacksovertimewithhistlegends.png",width=1000,height=3000)
grid.arrange(attacksbarplot,numattacksheat,killwoundbarplot,ncol=1,nrow=3)
dev.off()

# Barplot of number of attacks each day of the year
sumeventsnoyear = aggregate(eventid~imonth+iday,cleandata,FUN=length)
sumeventsnoyearo = sumeventsnoyear[order(sumeventsnoyear$imonth),]
sumeventsnoyearp = data.frame(sumeventsnoyearo,monthday=paste(sumeventsnoyearo$imonth,sumeventsnoyearo$iday,sep="-"))
sumeventsnoyearp$monthday = factor(sumeventsnoyearp$monthday,levels=sumeventsnoyearp$monthday[!duplicated(sumeventsnoyearp$monthday)])
daycolors = rep(1,length(sumeventsnoyearp$monthday)); daycolors[360] = 2; daycolors[60] = 3; daycolors[106] = 4; daycolors[1] = 5
eventsperdaymonth = ggplot(data=sumeventsnoyearp,aes(x=monthday,y=eventid,width=1,fill=factor(daycolors))) + geom_bar(stat="identity") + theme_bw(base_size=15) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks=element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank(),legend.position="none") + xlab("") + ylab("") + scale_fill_manual(values=c("#333333","#BB4A57","#956CB4","#5B9559","#B07A30"))
eventsperdaymonthcolorbar = ggplot(data=sumeventsnoyearp,aes(x=monthday,y=1)) + geom_tile(aes(fill=factor(imonth))) + theme_bw(base_size=15) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks=element_blank(), legend.position="none", panel.border=element_blank()) + xlab("") + ylab("") + scale_x_discrete(expand=c(0,0)) + scale_fill_manual(values=rep(c("#B6C8D5","#BDD683"),6))
png("./plots/eventsperdaymonthbarplot.png",width=1000,height=800)
grid.arrange(eventsperdaymonth,eventsperdaymonthcolorbar,ncol=1,nrow=2,heights=c(7,1))
dev.off()
cddayofweek = weekdays(as.Date(paste(cleandata$iday,cleandata$imonth,cleandata$iyear,sep="-"),"%d-%m-%Y"))
cleandatap = data.frame(cleandata,dayofweek=cddayofweek)
sumeventsweekday = aggregate(eventid~dayofweek,cleandatap,FUN=length)
sumeventsweekday$dayofweek = factor(sumeventsweekday$dayofweek,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
eventsperweekday = ggplot(data=sumeventsweekday,aes(x=dayofweek,y=eventid)) + geom_bar(stat="identity") + theme_bw(base_size=25) + xlab("") + ylab("")
png("./plots/eventsperweekday.png",width=1000,height=700)
eventsperweekday
dev.off()

# Line plot of types and targets of attacks over time
sumeventsbytype = aggregate(eventid~iyear+attacktype1,cleandata,FUN=length)
# Combine low count targets together
tempsumeventsbytarget = aggregate(eventid~iyear+targtype1,cleandata,FUN=length)
targettypecounts = aggregate(eventid~targtype1,tempsumeventsbytarget,FUN=sum); targetcombine = targettypecounts$targtype1[which(targettypecounts$eventid<=2500)]
cleandatat = cleandata; cleandatat$targtype1[cleandata$targtype1%in%targetcombine] = 99
sumeventsbytarget = aggregate(eventid~iyear+targtype1,cleandatat,FUN=length)
typecolors = c("#6187A8", "#CA5235", "#6BAB3F", "#C951CA", "#CD4079", "#9D7A31", "#4B8963", "#816CC1", "#9D5A71")
attacktypevstime = ggplot(data=sumeventsbytype,aes(x=iyear,y=eventid,color=as.factor(attacktype1))) + geom_line(size=2) + scale_color_manual(values=typecolors) + theme_bw(base_size=25) + xlab("") + ylab("")
png("./plots/attacktypeovertime.png",width=1000,height=700)
attacktypevstime
dev.off()
targetcolors = c("#C28E5A", "#BE58C9", "#70CF4B", "#4E4363", "#9EB8C1", "#C65987", "#88CF99", "#CAC347", "#4E6438", "#7C7ECB", "#CE4E39", "#63312B")
attacktargetvstime = ggplot(data=sumeventsbytarget,aes(x=iyear,y=eventid,color=as.factor(targtype1))) + geom_line(size=2) + scale_color_manual(values=targetcolors) + theme_bw(base_size=25) + xlab("") + ylab("")
png("./plots/attacktargetovertime.png",width=1000,height=700)
attacktargetvstime
dev.off()

# Heatmap of attacks stratified by each terrorist group over time
sumeventsbygroup = aggregate(eventid~iyear+gname,cleandata,FUN=length)
# Cluster matrix of group attacks
groupbyeventssums = dcast(sumeventsbygroup,gname~iyear,value.var="eventid")
rownames(groupbyeventssums) = groupbyeventssums[,1]; groupbyeventssums = groupbyeventssums[,-1]
groupbyeventssums[is.na(groupbyeventssums)] = 0 
colorsforgroupheat = colorRampPalette(brewer.pal(n = 7, name = "Blues"))(100)
groupclusterrowanno = data.frame(regions=as.character(cleandata$region_txt[match(rownames(groupbyeventssums),cleandata$gname)]))
rownames(groupclusterrowanno) = rownames(groupbyeventssums)
rowannocols = list(regions=c("#96A440", "#B15DD2", "#D64B33", "#51A292", "#5F5176", "#C8536C", "#C98C3A", "#4A622F", "#8A4831", "#7E8FCD", "#55B245", "#BF5499"))
names(rowannocols$regions) = unique(groupclusterrowanno$regions)
groupbyeventssumst = log10(groupbyeventssums+1)
png("./plots/clusteredattackgroups.png",width=2000,height=1500)
pheatmap(groupbyeventssumst,annotation_colors=rowannocols,clustering_method="ward",color=colorsforgroupheat,cluster_rows=TRUE,cluster_cols=FALSE,clustering_distance_rows="binary",show_rownames=FALSE,border_color=NA,annotation_row=groupclusterrowanno,fontsize=30)
dev.off()
# Number of attacks in each region
attacksbyregion = aggregate(eventid~region_txt,cleandata,FUN=length)
attacksbyregionbarplot = ggplot(data=attacksbyregion,aes(x=region_txt,y=eventid,width=.9,fill=factor(region_txt))) + geom_bar(stat="identity") + theme_bw(base_size=25) + theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank(),axis.text.x = element_text(angle = 75, hjust = 1)) + xlab("") + ylab("") + scale_fill_manual(values=rowannocols$regions)
png("./plots/attacksperregion.png",width=1000,height=700)
attacksbyregionbarplot
dev.off()
# Attacks over time in each region
attacksbyregionyear = aggregate(eventid~region_txt+iyear,cleandata,FUN=length)
attacksbyregionvstime = ggplot(data=attacksbyregionyear,aes(x=iyear,y=eventid,color=factor(region_txt))) + geom_line(size=2) + theme_bw(base_size=25) + theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank(),axis.text.x = element_text(angle = 75, hjust = 1)) + xlab("") + ylab("") + scale_color_manual(values=rowannocols$regions)
png("./plots/attackregionovertime.png",width=1000,height=700)
attacksbyregionvstime
dev.off()
# Number of unique groups operating each year
uniquegroupsfind <- function(x) {length(unique(x))}
groupsperyear = aggregate(gname~iyear,cleandata,FUN=uniquegroupsfind)
uniquegroupsbarplot = ggplot(data=groupsperyear,aes(x=as.factor(iyear),y=gname,width=.9)) + geom_bar(stat="identity") + theme_bw(base_size=50) + theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank(),axis.text.x = element_text(angle = 75, hjust = 1)) + xlab("") + ylab("")
png("./plots/uniquegroupsovertime.png",width=1000,height=700)
uniquegroupsbarplot
dev.off()

# Distribution of attacks per group
print(summary(apply(groupbyeventssums,1,sum)))
print(summary(apply(groupbyeventssums[-3052,],1,sum))) # Not including "unknown"


