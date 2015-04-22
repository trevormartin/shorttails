#######################################
#
# Program to analyze US foreign aid 
# over time
# See shorttails.io
#
#######################################

library(plyr)
library(ggplot2)
library(reshape2)
library(ggplot2)
library(ggbiplot)
library(scales)

##### Part 1: Load in the data

# Data is from Full_ForeignAssistanceData.xlsx where "planned" tab was copied to .csv using paste values only function
fulldata = read.csv("/Users/trevormartin/Dropbox/Short Tails/US Foreign Aid/fullforeignassistancedataplanned.csv",header=TRUE,stringsAsFactors=FALSE)

##### Part 2: Format the data into different matrices for each analysis

# Total for each country
countrytotals = ddply(fulldata,c("Fiscal.Year","Operating.Unit"),function(x) { total.amount=sum(x$Amount) })
countrytotalsmattemp = dcast(countrytotals,Operating.Unit~Fiscal.Year)
countrytotalsmat = countrytotalsmattemp # Replace NA with 0
countrytotalsmat[is.na(countrytotalsmat)] = 0
rownames(countrytotalsmat) = countrytotalsmattemp[,"Operating.Unit"]
countrytotalsmat = countrytotalsmat[,-1]
rownames(countrytotalsmat)[which(rownames(countrytotalsmat)=="N/A")] = "MCC/PeaceCorps"

# Total type of aid
typetotals = ddply(fulldata,c("Fiscal.Year","Category"),function(x) { total.amount=sum(x$Amount) })
typetotalsmattemp = dcast(typetotals,Category~Fiscal.Year)
typetotalsmat = typetotalsmattemp
rownames(typetotalsmat) = typetotalsmattemp[,"Category"]
typetotalsmat = typetotalsmat[,-1]
rownames(typetotalsmat)[1] = "Other" # Blank category is Peace Core and MCC

##### Part 3: Normalize the matrices

# Convert to percent of total for each year
countrytotalssum = apply(countrytotalsmat,2,sum)
countrytotalsmatnorm = sweep(countrytotalsmat,2,countrytotalssum,"/")
typetotalssum = apply(typetotalsmat,2,sum)
typetotalsmatnorm = sweep(typetotalsmat,2,typetotalssum,"/")

##### Part 4: Analysis of aid to each country over time

# Plot total aid over time
totalaidtime = data.frame(totals=countrytotalssum,years=names(countrytotalssum))
png("./plots/totalaidovertime.png",width=1000,height=500)
ggplot(aes(x=years,y=totals),data=totalaidtime) + geom_bar(stat="identity") + theme_bw(base_size=25) + scale_y_continuous(labels = dollar)
dev.off()

# Plot stacked barplot of percent aid over time
countrytotalsmatnorminvrank = apply(1/abs(countrytotalsmatnorm),2,rank) # Find top 5 countries by percent aid each year
countrytotalsmatnormtop = (countrytotalsmatnorminvrank<=5)
topkeeppull = which(apply(countrytotalsmatnormtop,1,sum)>=1)
topkeep = topkeeppull[order(apply(countrytotalsmatnormtop,1,sum)[topkeeppull],decreasing=TRUE)][1:7]
countrytotalsmatnormcollapse = rbind(countrytotalsmatnorm[topkeep,],apply(countrytotalsmatnorm[-topkeep,],2,sum))
rownames(countrytotalsmatnormcollapse)[nrow(countrytotalsmatnormcollapse)] = "All Other"
countrytotalsmatnormcollapsemelt = melt(as.matrix(countrytotalsmatnormcollapse))
png("./plots/percentaidovertime.png",width=1000,height=500)
ggplot(aes(x=Var2,y=value,fill=Var1),data=countrytotalsmatnormcollapsemelt) + geom_bar(stat="identity", position="fill") + scale_y_continuous(labels = percent_format()) + theme_bw(base_size=25) + theme(legend.position="none") + scale_fill_brewer(type="qual") + scale_x_continuous(breaks=2006:2015)
dev.off()
png("./plots/percentaidovertimelegend.png",width=500,height=500)
ggplot(aes(x=Var2,y=value,fill=Var1),data=countrytotalsmatnormcollapsemelt) + geom_bar(stat="identity", position="fill") + scale_y_continuous(labels = percent_format()) + theme_bw(base_size=15) + theme(legend.position="right") + scale_fill_brewer(type="qual")
dev.off()

# Plot scores of principal components
png("./plots/percentaidpc12.png",width=1000,height=1000)
percentaidpc = prcomp(scale(as.matrix(countrytotalsmatnorm)))
percentaidbiplot = ggbiplot(percentaidpc, obs.scale = 1, var.scale = 1, choices=1:2, circle = FALSE, var.axes=FALSE, labels=rownames(percentaidpc$x))
#percentaidbiplot = ggbiplot(percentaidpc, obs.scale = 1, var.scale = 1, choices=1:2, circle = FALSE, var.axes=FALSE, alpha=0.5) + geom_point(size=5, alpha=0.5)
percentaidbiplot + theme_bw(base_size=30) + theme(axis.text=element_text(size=35))
dev.off()

# Plot scores of principal components
png("./plots/percentaidpc23.png",width=1000,height=1000)
percentaidpc = prcomp(scale(as.matrix(countrytotalsmatnorm)))
percentaidbiplot = ggbiplot(percentaidpc, obs.scale = 1, var.scale = 1, choices=2:3, circle = FALSE, var.axes=FALSE, labels=rownames(percentaidpc$x))
#percentaidbiplot = ggbiplot(percentaidpc, obs.scale = 1, var.scale = 1, choices=2:3, circle = FALSE, var.axes=FALSE, alpha=0.5) + geom_point(size=5, alpha=0.5)
percentaidbiplot + theme_bw(base_size=30) + theme(axis.text=element_text(size=35))
dev.off()

# Plot biplot of principal components
png("./plots/percentaidpc12biplot.png",width=1000,height=1000)
percentaidpc = prcomp(scale(as.matrix(countrytotalsmatnorm)))
percentaidbiplot = ggbiplot(percentaidpc, obs.scale = 1, var.scale = 1, choices=1:2, circle = FALSE, var.axes=TRUE, alpha=0.5, varname.size=10, varname.adjust=10) + geom_point(size=5, alpha=0.5)
percentaidbiplot + theme_bw(base_size=30) + theme(axis.text=element_text(size=35))
dev.off()

# Find the top 10 increasing and top 10 decreasing percentage aid over time
corvals = vector("numeric",nrow(countrytotalsmatnorm))
for(i in 1:length(corvals)) {
    corvals[i] = cor(1:ncol(countrytotalsmatnorm),as.numeric(countrytotalsmatnorm[i,]))
}

# Top 10 increasing
top5up = rownames(countrytotalsmatnorm)[order(corvals,decreasing=TRUE)][1:5]
# Top 10 decreasing
top5down = rownames(countrytotalsmatnorm)[order(corvals,decreasing=FALSE)][1:5]

# Export full correlation list
countrytimeorder = order(corvals,decreasing=TRUE)
countrytimecor = data.frame(name=rownames(countrytotalsmatnorm)[countrytimeorder],correlation=corvals[countrytimeorder])
write.table(countrytimecor,file="countrytimecorrelations.txt",row.names=FALSE,quote=FALSE,sep="\t")

# Permute columns and recalculate correlations for null distribution
numperm = 100
permcorvals = vector("list",numperm)
for(j in 1:numperm) {
permcorvals[[j]] = vector("numeric",nrow(countrytotalsmatnorm))
    for(i in 1:length(permcorvals[[j]])) {
	permcorvals[[j]][i] = cor(1:ncol(countrytotalsmatnorm),as.numeric(countrytotalsmatnorm[i,sample(1:10,10)]))
    }
}
permcorvalstot = unlist(permcorvals)

# Plot real vs. null distributions of correlations on same plot
realnulldists = data.frame(vals=c(corvals,permcorvalstot),set=c(rep(1,length(corvals)),rep(2,length(permcorvalstot))))
png("./plots/realvsnullcor.png",width=1000,height=1000)
ggplot(aes(x=vals,fill=as.factor(set)),data=realnulldists) + geom_density(alpha=0.5) + theme_bw(base_size=30)
dev.off()

# Plot change in rank over time for top and bottom 10
countrytotalsmatnormranks = apply(countrytotalsmatnorm,2,rank)
top5uptop5down = c(top5up,top5down)
countrytotalsmatnormranksc = countrytotalsmatnormranks[match(top5uptop5down,rownames(countrytotalsmatnormranks)),]
countrytotalsmatnormrankscm = melt(countrytotalsmatnormranksc)
countrycolors = vector("character",length=nrow(countrytotalsmatnormrankscm))
countrycolors[!is.na(match(countrytotalsmatnormrankscm$Var1,top5up))] = "blue"
countrycolors[!is.na(match(countrytotalsmatnormrankscm$Var1,top5down))] = "red"
png("./plots/ranksovertime.png",width=1000,height=1000)
ggplot(aes(x=Var2,y=value,group=Var1),data=countrytotalsmatnormrankscm) + geom_line(color=countrycolors, size=3) + theme_bw(base_size=30) + scale_x_continuous(breaks=2006:2015) + theme(axis.ticks.y=element_blank())
dev.off()

##### Part 5: Analysis of type of aid over time

# Plot stacked barplot of percent type aid over time
typetotalsmatnormmelt = melt(as.matrix(typetotalsmatnorm))
png("./plots/percenttypeaidovertime.png",width=1000,height=500)
ggplot(aes(x=Var2,y=value,fill=Var1),data=typetotalsmatnormmelt) + geom_bar(stat="identity", position="fill") + scale_y_continuous(labels = percent_format()) + theme_bw(base_size=25) + theme(legend.position="none") + scale_fill_brewer(type="qual",palette="Set3") + scale_x_continuous(breaks=2006:2015)
dev.off()
png("./plots/percenttypeaidovertimelegend.png",width=500,height=500)
ggplot(aes(x=Var2,y=value,fill=Var1),data=typetotalsmatnormmelt) + geom_bar(stat="identity", position="fill") + scale_y_continuous(labels = percent_format()) + theme_bw(base_size=15) + theme(legend.position="right") + scale_fill_brewer(type="qual",palette="Set3")
dev.off()
