#######################################
#
# Program to analyze relationships
# between admin, tuition, and student growth
# See shorttails.io
#
#######################################

library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(gplots)
library(scales)
library(XML)
library(lubridate)

##### Part 1: Load in the data

# Downloading the administrator and student growth data
pageurl = "http://college-table.wgbh.org/college_local"
pagetables = readHTMLTable(pageurl,header=TRUE)
adminstudgrwraw = data.frame(pagetables$data)

# Loading in the college tuition data downloaded from http://nces.ed.gov/ipeds/datacenter/
collegetuitionsraw = read.csv("TrendData.csv",stringsAsFactors=FALSE)

# Loading in consumer price index data
cpidata = read.csv("http://research.stlouisfed.org/fred2/data/CPIAUCSL.csv", header = TRUE)

##### Part 2: Format and clean up the data into different matrices for each analysis

# Administrator and student growth data
adminstudgrwchar = data.frame(lapply(adminstudgrwraw,as.character),stringsAsFactors=FALSE)
adminstudgrwnona = adminstudgrwchar[(apply((adminstudgrwchar=="NA"),1,sum)==0),] # Remove colleges with missing data
adminstudgrwnona2 = adminstudgrwnona
for(i in c(3,4,6,7,9,10)) {
adminstudgrwnona2[,i] = as.numeric(adminstudgrwnona[,i])
}
adminstudgrwnonac = adminstudgrwnona2[which(adminstudgrwnona2[,9]>=500 & adminstudgrwnona2[,10]>=500),] # Remove colleges with less than 1000 enrolled students at either time point
netadminold = adminstudgrwnonac[,3] + adminstudgrwnonac[,6] # Calculate net administrator/professional fold change
netadminnew = adminstudgrwnonac[,4] + adminstudgrwnonac[,7]
netadminfc = netadminnew/netadminold
studfc = adminstudgrwnonac[,10]/adminstudgrwnonac[,9]
adminstudgrwclean = data.frame(adminstudgrwnonac,netadminold,netadminnew,netadminfc,studfc)

# College tuition data
collegetuitionsc = collegetuitionsraw[(apply((collegetuitionsraw=="N/A"),1,sum)==0),] # Remove colleges with any missing data
collegetuitionsclean = collegetuitionsc
for(i in c(3:17)) {
collegetuitionsclean[,i] = as.numeric(gsub(",","",collegetuitionsc[,i]))
}
cpidata$cpi_year = year(cpidata$DATE) # Adjust values for inflation (thanks to tips from http://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package)
yearlycpi = cpidata %>% group_by(cpi_year) %>% summarize(cpi = mean(VALUE))
yearlycpi$adj_factor = yearlycpi$cpi/yearlycpi$cpi[yearlycpi$cpi_year == 2013]
collegetuitionsinfadjc = scale(collegetuitionsclean[,3:17],center=FALSE,scale=yearlycpi$adj_factor[match(c(1999:2013),yearlycpi$cpi_year)])
collegetuitionsinfadj = data.frame(collegetuitionsclean[,1:2],collegetuitionsinfadjc)
matchtuitiongrowth = match(collegetuitionsinfadj$Institution.Name,adminstudgrwclean$School.Name) # Match up to administrator and growth data
collegetuitionsmatched = collegetuitionsinfadj[!is.na(matchtuitiongrowth),]
adminstudgrwcleanmatch = adminstudgrwclean[matchtuitiongrowth[!is.na(matchtuitiongrowth)],]

# Save the data (since some is scraped from websites)
save(adminstudgrwclean,collegetuitionsmatched,adminstudgrwcleanmatch,file="adminstudenttuitiondata.rdata")

##### Part 3: Analysis of administrative and student increases over time

# Linear plot of student vs. admin fold changes
adminstudmean = data.frame(adminmean=mean(adminstudgrwclean$netadminfc),studmean=mean(adminstudgrwclean$studfc))
png("./plots/adminvsstudfc.png",width=1000,height=1000)
ggplot(data=adminstudgrwclean) + geom_point(aes(x=netadminfc,y=studfc),alpha=.8,size=5) + theme_bw(base_size=35) + scale_x_continuous(breaks=seq(0,40,10),limits=c(0,45)) + scale_y_continuous(breaks=seq(0,40,10),limits=c(0,45)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank()) + geom_abline(intercept=0,slope=1,col="red") + geom_point(data=adminstudmean,aes(x=adminmean,y=studmean),col="red",size=6,pch=15)
#ggplot(data=adminstudgrwclean) + geom_point(aes(x=netadminfc,y=studfc),alpha=.8,size=5) + theme_bw(base_size=35) + scale_x_continuous(breaks=seq(0,40,10),limits=c(0,45)) + scale_y_continuous(breaks=seq(0,40,10),limits=c(0,45)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank()) + geom_abline(intercept=0,slope=1,col="red") + geom_point(data=adminstudmean,aes(x=adminmean,y=studmean),col="red",size=6,pch=15) + geom_text(aes(x=netadminfc,y=studfc,label=ifelse(netadminfc>20 | studfc>10,as.character(School.Name),"")),vjust=0)
dev.off()

# Log plot of student vs. admin fold changes
png("./plots/log10adminvsstudfc.png",width=1000,height=1000)
ggplot(data=adminstudgrwclean) + geom_point(aes(x=log10(netadminfc),y=log10(studfc),size=log10(Enrollment.Total..2011.)),col="black",alpha=.65) + theme_bw(base_size=35) + scale_x_continuous(breaks=seq(-1,2,0.5),limits=c(-1,2)) + scale_y_continuous(breaks=seq(-1,2,0.5),limits=c(-1,2)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank(),legend.position="none") + geom_abline(intercept=0,slope=1,col="red") + geom_point(data=adminstudmean,aes(x=log10(adminmean),y=log10(studmean)),col="red",size=5,lwd=5,pch=15) 
#ggplot(data=adminstudgrwclean) + geom_point(aes(x=log10(netadminfc),y=log10(studfc),size=log10(Enrollment.Total..2011.)),col="black",alpha=.65) + theme_bw(base_size=25) + scale_x_continuous(breaks=seq(-1,2,0.5),limits=c(1,2)) + scale_y_continuous(breaks=seq(-1,2,0.5),limits=c(-1,2)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank(),legend.position="none") + geom_abline(intercept=0,slope=1,col="red") + geom_point(data=adminstudmean,aes(x=log10(adminmean),y=log10(studmean)),col="red",size=5,lwd=5,pch=15) + geom_text(aes(x=log10(netadminfc),y=log10(studfc),label=ifelse(log10(netadminfc)>1,as.character(School.Name),"")),vjust=0)
dev.off()

# Correlation on log scale between student and admin fold changes
print(cor(log10(adminstudgrwclean$netadminfc),log10(adminstudgrwclean$studfc)))

# Colleges with decrease in students and increase in admin
print(adminstudgrwclean[which(log10(adminstudgrwclean$netadminfc)>0 & log10(adminstudgrwclean$studfc)<0),"School.Name"])
# Colleges with increase in students and decrease in admin
print(adminstudgrwclean[which(log10(adminstudgrwclean$netadminfc)<0 & log10(adminstudgrwclean$studfc)>0.5),"School.Name"])

# Write table of colleges ranked by admin fold change/student fold change (those with highest admin growth relative to student on top)
adminstudfcforwrite = data.frame(adminstudgrwclean[,c("School.Name","State","netadminold","netadminnew","Enrollment.Total..1987.","Enrollment.Total..2011.","netadminfc","studfc")],relfc=(adminstudgrwclean$netadminfc/adminstudgrwclean$studfc))
colnames(adminstudfcforwrite) = c("School.Name","State","Admin1987","Admin2011","Student1987","Student2011","AdminFC","StudentFC","AdmintoStudentFCRatio")
adminstudfcforwriteo = adminstudfcforwrite[order(adminstudfcforwrite$AdmintoStudentFCRatio,decreasing=TRUE),]
write.table(adminstudfcforwriteo,file="adminvsstudentgrowth.txt",quote=FALSE,row.names=FALSE,col.names=TRUE,sep="\t")

# Average ratio between administration and student fold change increases
print(mean(adminstudfcforwrite$AdmintoStudentFCRatio))

# Write HTML table of top 10 colleges ranked by admin to student fold change ratio
adminstudfcforhtml = adminstudfcforwriteo[1:10,-1]
rownames(adminstudfcforhtml) = adminstudfcforwriteo[1:10,1]
sink("adminstudentfchtmltable.html")
print(xtable(adminstudfcforhtml,display=c("s","s","d","d","d","d","f","f","f")),type="html")
sink()

##### Part 4: Analysis of relationships between tuition and administrative/student numbers

# Plot of number of administrators versus college tuition (thanks to tips from http://stackoverflow.com/questions/8545035/scatterplot-with-marginal-histograms-in-ggplot2)
matchforplot = data.frame(adminstudgrwcleanmatch,collegetuitionsmatched)
admintuitionscatter = ggplot(data=matchforplot) + geom_point(aes(x=netadminnew,y=X2011.12,size=log10(Enrollment.Total..2011.)),alpha=.8,col="black") + theme_bw(base_size=35) + scale_x_continuous() + scale_y_continuous(breaks=seq(0,50000,10000),limit=c(0,50000)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank(),legend.position="none") + geom_hline(yintercept=median(matchforplot$X2011.12),col="red",lty=2)
#admintuitionscatter = ggplot(data=matchforplot) + geom_point(aes(x=netadminnew,y=X2011.12),alpha=.8,size=5,col="black") + theme_bw(base_size=35) + scale_x_continuous() + scale_y_continuous(breaks=seq(0,50000,10000),limit=c(0,50000)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank(),legend.position="none") + geom_hline(yintercept=median(matchforplot$X2011.12),col="red",lty=2) + geom_text(aes(x=netadminnew,y=X2011.12,label=ifelse(netadminnew>5000,as.character(School.Name),"")),vjust=0)
tuitionhist = ggplot(data=matchforplot) + geom_histogram(aes(x=X2011.12),binwidth=1000) + theme_bw(base_size=35) + scale_x_continuous(breaks=seq(0,50000,10000),limit=c(0,50000)) + scale_y_continuous(breaks=seq(0,150,75)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank(),legend.position="none",axis.title.y=element_blank(),axis.text.y=element_blank()) + coord_flip() + geom_vline(xintercept=median(matchforplot$X2011.12),col="red",lty=2)
png("./plots/numadminvscollegetuition.png",width=1000,height=1000)
grid.arrange(admintuitionscatter,tuitionhist,ncol=2,nrow=1,widths=c(4,1))
dev.off()

# Plot of administrator to student ratio versus college tuition
studtoadmintuitionscatter = ggplot(data=matchforplot) + geom_point(aes(x=(netadminnew/Enrollment.Total..2011.),y=X2011.12,size=log10(Enrollment.Total..2011.)),alpha=.8,col="black") + theme_bw(base_size=35) + scale_x_continuous() + scale_y_continuous(breaks=seq(0,50000,10000),limit=c(0,50000)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank(),legend.position="none") + geom_hline(yintercept=median(matchforplot$X2011.12),col="red",lty=2)
studtoadmintuitionhist = ggplot(data=matchforplot) + geom_histogram(aes(x=X2011.12),binwidth=1000) + theme_bw(base_size=35) + scale_x_continuous(breaks=seq(0,50000,10000),limit=c(0,50000)) + scale_y_continuous(breaks=seq(0,150,75)) + theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.border = element_blank(),legend.position="none",axis.title.y=element_blank(),axis.text.y=element_blank()) + coord_flip() + geom_vline(xintercept=median(matchforplot$X2011.12),col="red",lty=2)
png("./plots/adminstudratiovscollegetuition.png",width=1000,height=1000)
grid.arrange(studtoadmintuitionscatter,studtoadmintuitionhist,ncol=2,nrow=1,widths=c(4,1))
dev.off()

# Look at which colleges fall in the two groups of high administrator colleges
print(collegetuitionsmatched[which(collegetuitionsmatched[,15]>30000 & adminstudgrwcleanmatch$netadminnew>2000),"Institution.Name"])
print(collegetuitionsmatched[which(collegetuitionsmatched[,15]<15000 & adminstudgrwcleanmatch$netadminnew>2000),"Institution.Name"])

# Look at which colleges fall in the group of high administrator ratio and high tuition colleges
pullhighadminhighcostcolleges = collegetuitionsmatched[which(collegetuitionsmatched[,15]>35000 & (adminstudgrwcleanmatch$netadminnew/adminstudgrwcleanmatch$Enrollment.Total..2011.)>0.2),"Institution.Name"]
print(pullhighadminhighcostcolleges)

# Write HTML table of colleges with high administrator ratio and high tuition
matchforwritehtml = data.frame(matchforplot[,c("School.Name","State","netadminnew","Enrollment.Total..2011.","X2011.12")],adminstudratio=(matchforplot$netadminnew/matchforplot$Enrollment.Total..2011.))
colnames(matchforwritehtml) = c("School.Name","State","Admin2011","Student2011","Tuition2011","AdmintoStudentRatio")
matchforwritehtmlc = matchforwritehtml[match(pullhighadminhighcostcolleges,matchforwritehtml$School.Name),]
matchforwritehtmlo = matchforwritehtmlc[order(matchforwritehtmlc$AdmintoStudentRatio,decreasing=TRUE),]
matchforwritehtmlohtml = matchforwritehtmlo[,-1]
rownames(matchforwritehtmlohtml) = matchforwritehtmlo[,1]
sink("hightuitionadminstudenratiohtmltable.html")
print(xtable(matchforwritehtmlohtml,display=c("s","s","d","d","d","f")),type="html")
sink()

# Write table of colleges ranked by admin count/tuition dollar (admins per tuition dollar)
matchforwrite = data.frame(matchforplot[,c("School.Name","State","netadminnew","X2011.12")],adminperdollar=(matchforplot$netadminnew/matchforplot$X2011.12))
colnames(matchforwrite) = c("School.Name","State","Admin2011","Tuition2011","AdminperDollar")
matchforwriteo = matchforwrite[order(matchforwrite$AdminperDollar,decreasing=TRUE),]
write.table(matchforwriteo,file="adminvstuition.txt",quote=FALSE,row.names=FALSE,col.names=TRUE,sep="\t")

