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
library(maps)
library(geosphere)

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

# Plot GDP per capita over time for each country highlighting most increased and most decreased
mostincreased = names(which.max(gdppercapnorm[,23]-gdppercapnorm[,1]))
mostincreased
mostdecreased = names(which.min(gdppercapnorm[,23]-gdppercapnorm[,1]))
mostdecreased
gdppercaptime = melt(gdppercapnorm)
png("./plots/gdppercapovertime.png",width=1000,height=500)
ggplot(data=gdppercaptime) + geom_line(aes(x=Var2,y=value,group=Var1),col="grey") + geom_line(aes(x=Var2,y=value,group=Var1),subset=.(Var1%in%mostincreased),col="blue",size=2) + geom_line(aes(x=Var2,y=value,group=Var1),subset=.(Var1%in%mostdecreased),col="red",size=2) + theme_bw(base_size=25) + scale_y_log10(labels = dollar)
dev.off()

# Plot GDP per capita growth over time for each country
gdppercapgrowtime = melt(gdppercapgrownorm)
png("./plots/gdppercapgrowthovertime.png",width=1000,height=500)
ggplot(aes(x=Var2,y=value/100,group=Var1),data=gdppercapgrowtime) + geom_line() + theme_bw(base_size=25) + scale_y_continuous(labels = percent)
dev.off()

# Calculate all country pairwise correlations
gdppercapcors = cor(t(gdppercapnorm),method="spearman")
gdppercapgrowcors = cor(t(gdppercapgrownorm),method="spearman")
gdppercapcorsv = gdppercapcors[lower.tri(gdppercapcors)]
gdppercapgrowcorsv = gdppercapgrowcors[lower.tri(gdppercapgrowcors)]
# Get indexes of matrices
gdppercapcorsind = which(lower.tri(gdppercapcors),arr.ind=TRUE)
gdppercapgrowcorsind = which(lower.tri(gdppercapgrowcors),arr.ind=TRUE)
gdppercapcorsindn = gdppercapcorsind; gdppercapgrowcorsindn = gdppercapgrowcorsind
gdppercapcorsindn[,1] = rownames(gdppercapcors)[gdppercapcorsind[,1]]
gdppercapcorsindn[,2] = colnames(gdppercapcors)[gdppercapcorsind[,2]]
gdppercapgrowcorsindn[,1] = rownames(gdppercapgrowcors)[gdppercapgrowcorsind[,1]]
gdppercapgrowcorsindn[,2] = colnames(gdppercapgrowcors)[gdppercapgrowcorsind[,2]]
# Extract long and lat coords for each pairwise comparison
gdppercaplonglats = cbind(gdppercapc[match(gdppercapcorsindn[,1],gdppercapc$country),c("longitude","latitude")],gdppercapc[match(gdppercapcorsindn[,2],gdppercapc$country),c("longitude","latitude")])
gdppercapgrowlonglats = cbind(gdppercapgrowc[match(gdppercapgrowcorsindn[,1],gdppercapgrowc$country),c("longitude","latitude")],gdppercapgrowc[match(gdppercapgrowcorsindn[,2],gdppercapgrowc$country),c("longitude","latitude")])

# Plot top 50 and bottom 50 pairwise correlations on world map
curres = 100
curpal = colorRampPalette(c("red", "grey","blue"))
plotcolors = curpal(curres)
plotalphas = seq(0.05,1,length.out=curres)
png("./plots/allcorrelations.png",width=1000,height=750)
map("world",fill=TRUE,col=rgb(0.3,0.3,0.3,0.5))
# Plot great circles on map
for (i in 1:nrow(gdppercaplonglats)) {
    start.cord = as.numeric(as.matrix(gdppercaplonglats[i,1:2]))
    end.cord = as.numeric(as.matrix(gdppercaplonglats[i,3:4]))
    gci = gcIntermediate(p1=start.cord, p2=end.cord, addStartEnd=TRUE, breakAtDateLine=TRUE)
    curpercentile = round(((gdppercapcorsv[i]+1)/max(gdppercapcorsv+1))*curres)
    curcol = alpha(plotcolors[curpercentile],plotalphas[curpercentile])
    if (is.list(gci) == TRUE)
    lines(gci[[1]], col=curcol)
    else lines(gci, col=curcol)
}
dev.off()



    if (is.list(gci) == TRUE)
    lines(gci[[1]], col=rgb(1 - gdppercapcorsv[i],0, gdppercapcorsv[i],0.01)) 
    else lines(gci, col=rgb(1 - gdppercapcorsv[i],0, gdppercapcorsv[i],0.01))





##### code to transform to standard normal
# Transform each gene to standard normal
cleanexpdata = vector("list",length(expinfo))
for(i in 1:length(expinfo) ) {
	mat = cleanexpdatatemp[[i]]
	mat2 = t(apply(mat, 1, rank, ties.method = "average"))
	mat3 = qnorm(mat2 / (ncol(mat)+1))
	cleanexpdata[[i]] = mat3
}

