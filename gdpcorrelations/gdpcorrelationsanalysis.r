#######################################
#
# Program to analyze worldwide GDP
# correlations over time
# See shorttails.io
#
#######################################

library(plyr)
library(ggplot2)
library(reshape2)
library(ggplot2)
library(ggbiplot)
library(scales)
library(WDI)
library(preprocessCore)

##### Part 1: Load in the data

# Data is from the World Development Index WDI package
# We use the search function to pull out all entries dealing with GDP
pullterms = WDIsearch(string="gdp")
# Extracted interesting rows
pulltermsc = pullterms[c(42,54,86,87,93,95),]
# Here we are interested in GDP per capita in current dollars and annual growth percent
pulltermscc = pulltermsc[c(5,6),1]

# Thanks to http://www.r-bloggers.com/what-value-is-cross-country-gdp-correlation-part-one/ for great tips on what years to look at and data formatting
gdppercap = WDI(country="all",indicator="NY.GDP.PCAP.CD",start=1991,end=2013,extra=TRUE)
gdppercapgrow = WDI(country="all",indicator="NY.GDP.PCAP.KD.ZG",start=1991,end=2013,extra=TRUE)

##### Part 2: Format the data into different matrices for each analysis

gdppercapc = subset(gdppercap, gdppercap$region != "Aggregates")
gdppercapgrowc = subset(gdppercapgrow, gdppercapgrow$region != "Aggregates")
gdppercapcast = recast(gdppercapc, id.var=c("country","year"),measure.var="NY.GDP.PCAP.CD",formula = year ~ country)
gdppercapgrowcast = recast(gdppercapgrowc, id.var=c("country","year"),measure.var="NY.GDP.PCAP.KD.ZG",formula = year ~ country)
# Remove any countries that have missing data
gdppercapna = apply(gdppercapcast,2,function(x) { any(is.na(x)) })
gdppercapgrowna = apply(gdppercapgrowcast,2,function(x) { any(is.na(x)) })
gdppercapclean = gdppercapcast[,!gdppercapna]
gdppercapgrowclean = gdppercapgrowcast[,!gdppercapgrowna]
gdppercapclean2 = t(gdppercapclean)
gdppercapgrowclean2 = t(gdppercapgrowclean)
colnames(gdppercapclean2) = gdppercapclean2[1,]
colnames(gdppercapgrowclean2) = gdppercapgrowclean2[1,]
gdppercapclean3 = gdppercapclean2[-1,]
gdppercapgrowclean3 = gdppercapgrowclean2[-1,]

##### Part 3: Normalize the matrices

# Normalize the matrices with quantile normalization (same distribution each year)
gdppercapnorm = normalize.quantiles(gdppercapclean3)
colnames(gdppercapnorm) = colnames(gdppercapclean3); rownames(gdppercapnorm) = rownames(gdppercapclean3)
gdppercapgrownorm = normalize.quantiles(gdppercapgrowclean3)
colnames(gdppercapgrownorm) = colnames(gdppercapgrowclean3); rownames(gdppercapgrownorm) = rownames(gdppercapgrowclean3)

##### Part 4: Analysis of correlations between GDP over time

# Plot GDP per capita over time for each country


# Plot GDP per capita growth over time for each country







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
