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

# Function to make world map plots
makeworldmapplot <- function(longlatmat,valsvec,curresval,colsvec,alphasvec,lwdsvec) {
    map("world",fill=TRUE,col=rgb(0.3,0.3,0.3,0.5))
    # Plot great circles on map
    for (i in 1:nrow(longlatmat)) {
	start.cord = as.numeric(as.matrix(longlatmat[i,1:2]))
	end.cord = as.numeric(as.matrix(longlatmat[i,3:4]))
	gci = gcIntermediate(p1=start.cord, p2=end.cord, addStartEnd=TRUE, breakAtDateLine=TRUE)
	curpercentile = round(((valsvec[i]+1)/max(valsvec+1))*curresval)
	curabspercentile = round((abs(valsvec[i])/max(abs(valsvec)))*curresval)
	curcol = alpha(colsvec[curpercentile],alphasvec[curabspercentile])
	curlwd = lwdsvec[curabspercentile]
	if (is.list(gci) == TRUE)
	lines(gci[[1]], col=curcol, lwd=curlwd)
	else lines(gci, col=curcol, lwd=curlwd)
    }
}

# Plot all correlations on world map
curres = 100
curpal = colorRampPalette(c("red", "grey","blue"))
plotcolors = curpal(curres)
plotalphas = exp(seq(log(0.05),log(1),length.out=curres))
plotlwds = exp(seq(log(0.05),log(1),length.out=curres))
# Order matrices by absolute strength of correlation
absso = order(abs(gdppercapcorsv),decreasing=FALSE)
gdppercaplonglatso = gdppercaplonglats[absso,]
gdppercapcorsvo = gdppercapcorsv[absso]
gdppercapcorsindno = gdppercapcorsindn[absso,]
png("./plots/allcorrelations.png",width=10000,height=7500)
makeworldmapplot(gdppercaplonglatso,gdppercapcorsvo,curres,plotcolors,plotalphas,plotlwds)
dev.off()

# Plot all growth correlations on world map
curresg = 100
curpal = colorRampPalette(c("red", "grey","blue"))
plotcolorsg = curpal(curresg)
plotalphasg = (seq((0.05),(1),length.out=curresg))
plotlwdsg = (seq((0.05),(1),length.out=curresg))
# Order matrices by absolute strength of correlation
abssogrow = order(abs(gdppercapgrowcorsv),decreasing=FALSE)
gdppercapgrowlonglatso = gdppercapgrowlonglats[abssogrow,]
gdppercapgrowcorsvo = gdppercapgrowcorsv[abssogrow]
gdppercapgrowcorsindno = gdppercapgrowcorsindn[abssogrow,]
png("./plots/allcorrelationsgrow.png",width=10000,height=7500)
makeworldmapplot(gdppercapgrowlonglatso,gdppercapgrowcorsvo,curresg,plotcolorsg,plotalphasg,plotlwdsg)
dev.off()

# Plot top 10 and bottom 10 growth correlations on map
plotalphasgtb = rep(0.9,curresg)
plotlwdsgtb = rescale(10000^(seq(1,5,length.out=curresg)),c(10,25))
topposcor = which(gdppercapgrowcorsvo>0)[which(rank(1/gdppercapgrowcorsvo[gdppercapgrowcorsvo>0])<=10)]
topnegcor = which(gdppercapgrowcorsvo<0)[which(rank(abs(1/gdppercapgrowcorsvo[gdppercapgrowcorsvo<0]))<=10)]
gdppercapgrowlonglatsotb = gdppercapgrowlonglatso[c(topposcor,topnegcor),]
gdppercapgrowcorsvotb = gdppercapgrowcorsvo[c(topposcor,topnegcor)]
png("./plots/allcorrelationsgrowtb.png",width=10000,height=7500)
makeworldmapplot(gdppercapgrowlonglatsotb,gdppercapgrowcorsvotb,curresg,plotcolorsg,plotalphasgtb,plotlwdsgtb)
dev.off()

# Write lists of pairwise correlations
allcorrelationswrite = cbind(gdppercapcorsindno,gdppercapcorsvo)
colnames(allcorrelationswrite) = c("country1","country2","spearman_cor")
allcorrelationswrite2 = allcorrelationswrite[nrow(allcorrelationswrite):1,]
write.table(allcorrelationswrite2,file="allcorrelations.txt",quote=FALSE,row.names=FALSE,col.names=TRUE,sep="\t")
allcorrelationswritegrow = cbind(gdppercapgrowcorsindno,gdppercapgrowcorsvo)
colnames(allcorrelationswritegrow) = c("country1","country2","spearman_cor")
allcorrelationswritegrow2 = allcorrelationswritegrow[nrow(allcorrelationswritegrow):1,]
write.table(allcorrelationswritegrow2,file="allcorrelationsgrow.txt",quote=FALSE,row.names=FALSE,col.names=TRUE,sep="\t")




##### code to transform to standard normal
# Transform each gene to standard normal
cleanexpdata = vector("list",length(expinfo))
for(i in 1:length(expinfo) ) {
	mat = cleanexpdatatemp[[i]]
	mat2 = t(apply(mat, 1, rank, ties.method = "average"))
	mat3 = qnorm(mat2 / (ncol(mat)+1))
	cleanexpdata[[i]] = mat3
}

