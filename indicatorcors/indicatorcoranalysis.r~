#######################################
#
# Program to analyze links between
# different global indicators
# See shorttails.io
#
#######################################

library(ggplot2)
library(reshape2)
library(qvalue)
library(gplots)
library(RColorBrewer)

##### Part 1: Load in the data

filenames = list.files("spreadsheets", pattern="*", full.names=TRUE)

# Adapted from the Datamart R package
readinspreadsheets <- function(x) {
    res <- read.table(x, na.strings = c("..", "-"), stringsAsFactor = FALSE, sep="\t", quote="", header=TRUE, fill=TRUE)
    #     tm <- try(as.Date(paste(substring(tail(colnames(res), -1), 2), 1, 1, sep = "-0")), silent = TRUE)
    tm <- try(as.Date(paste(substring(tail(colnames(res), -1), 2), 1, 1, sep = "-0")), silent = TRUE)
    if (!inherits(tm, "try-error")) {
	curspreaddesc = colnames(res)[1]
        colnames(res)[1] = "country"
	#         res <- reshape(res, idvar = "country", varying = list(2:ncol(res)), times = strtail(colnames(res)[2:ncol(res)], -1), v.names = resource, direction = "long")
	#         res <- res[!is.na(res[, resource]), ]
	for(j in 2:ncol(res)) {
	    res[,j] = as.numeric(gsub(",", "", sub(")", "", sub("(", "", res[,j], fixed=TRUE), fixed=TRUE), fixed=TRUE))
	}
    resfull = list(res,curspreaddesc)
    return(resfull)
    }
    if (inherits(tm, "try-error")) {
    resfull = list(NA,NA)
    return(resfull)
    }
}

allspreaddata = vector("list",length(filenames))
allspreadnames = vector("character",length(filenames))
for(i in 1:length(filenames)) {
    curres = readinspreadsheets(filenames[i])
    allspreaddata[[i]] = curres[[1]]
    allspreadnames[i] = curres[[2]]
}

# Limit analysis to only data sets with at least 20 years of data
yearthresh = 20
spreadyears = (unlist(lapply(allspreaddata,ncol))-1)
allspreaddatac = allspreaddata[spreadyears>=yearthresh]
allspreadnamesc = allspreadnames[spreadyears>=yearthresh]
namethresh = (allspreadnamesc%in%c("X","Country","Row.Labels"))
allspreaddatacc = allspreaddatac[!namethresh]
allspreadnamescc = allspreadnamesc[!namethresh]

##### Part 2: Format and clean up the data

# Convert data to ranks for each country



##### Part 3: Analysis of correlations between indicators


# Look at correlations between indicators for each country across years
# Take median of country correlations as measure of indicator correlation
# Heatmap indicator correlations

# Could also take first PC of each indicator matrix and just correlate those







1+1
# Candidate information
candinfo = read.table("cn.txt",stringsAsFactors=FALSE,sep="|",quote="",header=FALSE,comment.char="")
candheader = read.csv("cn_header_file.csv")
colnames(candinfo) = colnames(candheader)
# Committee information
cominfo = read.table("cm.txt",stringsAsFactors=FALSE,sep="|",quote="",header=FALSE,comment.char="")
comheader = read.csv("cm_header_file.csv")
colnames(cominfo) = colnames(comheader)
# Committee to candidate contribution information
comtocand = read.table("itpas2.txt",stringsAsFactors=FALSE,sep="|",quote="",header=FALSE,comment.char="")
comtocandheader = read.csv("pas2_header_file.csv")
colnames(comtocand) = colnames(comtocandheader)

##### Part 2: Format and clean up the data for distance analysis

comtocandclean = comtocand[-which(comtocand$CAND_ID==""),] # Remove transactions without candidate ID
comtocandcleanc = comtocandclean[,c("CMTE_ID","CAND_ID","TRANSACTION_AMT")]
comtocandcleancm = melt(comtocandcleanc)
comtocandcleanmat = dcast(comtocandcleancm, CMTE_ID~CAND_ID,fun.aggregate=sum)
comtocandcleanmatf = comtocandcleanmat[,-1]
rownames(comtocandcleanmatf) = comtocandcleanmat[,1]
comtocandcleanmatfb = 1*(comtocandcleanmatf>0) # Binary version of matrix
print(summary(apply(comtocandcleanmatfb,1,sum))) # Distribution of committee unique candidate contributions
print(summary(apply(comtocandcleanmatfb,2,sum))) # Distribution of candidate unique committee contributions

##### Part 3: Analysis of associations between political campaigns through committee funding

# Code for calculating jaccard coefficient on sparse matrices
# Thanks to http://stats.stackexchange.com/questions/49453/calculating-jaccard-or-other-association-coefficient-for-binary-data-using-matri
jaccard <- function(m) {
    ## common values:
    A = tcrossprod(m)
    ## indexes for non-zero common values
    im = which(A > 0, arr.ind=TRUE)
    ## counts for each row
    b = rowSums(m)

    ## only non-zero values of common
    Aim = A[im]

    ## Jacard formula: #common / (#i + #j - #common)
    J = sparseMatrix(
          i = im[,1],
          j = im[,2],
          x = Aim / (b[im[,1]] + b[im[,2]] - Aim),
          dims = dim(A)
    )

    return( J )
}

# Use jaccard function to calculate jaccard coefficients on real data
comtocandjac = as.matrix(jaccard(t(comtocandcleanmatfb)))

# Create 1000 random matrices and calculate jaccard coefficients for empirical pvalues
numperm = 1000
jacpvalstore = matrix(0,nrow=nrow(comtocandjac),ncol=ncol(comtocandjac))
for(i in 1:numperm) {
	print(i)
	curpermmat = apply(comtocandcleanmatfb,2,sample)
	comtocandjacperm = jaccard(t(curpermmat))
	jacpvalstore = jacpvalstore + (1*(comtocandjacperm>=comtocandjac))
}
jacpvalmat = (jacpvalstore+1)/(numperm+1)
jacpvals = jacpvalmat[lower.tri(jacpvalmat)]
jacqvals = qvalue(jacpvals)$qvalues
jacindexes = which(lower.tri(jacpvalmat),arr.ind=TRUE)
jaccoef = comtocandjac[lower.tri(comtocandjac)]
# Use an FDR of 5%
jaccut = which(jacqvals<=0.05)
siginfo = cbind(pvals=jacpvals[jaccut],qvals=jacqvals[jaccut],coef=jaccoef[jaccut],indexes=jacindexes[jaccut,])
# Rank by number of contributions candidates had weighted by jaccard coefficient
contsizes = apply(comtocandcleanmatfb,2,sum)
siginfo2 = data.frame(siginfo,cand1cont=contsizes[jacindexes[jaccut,1]],cand2cont=contsizes[jacindexes[jaccut,2]])
siginfo2o = siginfo2[order(siginfo2$coef*(rank(siginfo2$cand1cont)+rank(siginfo2$cand2cont)),decreasing=TRUE),]
# Annotate significant associations
cand1ids = colnames(comtocandcleanmatf)[siginfo2o$row]
cand2ids = colnames(comtocandcleanmatf)[siginfo2o$col]
siginfo3 = data.frame(siginfo2o,totcont=(siginfo2o$cand1cont+siginfo2o$cand2cont),cand1id=cand1ids,cand2id=cand2ids)
cand1match = match(cand1ids,candinfo$CAND_ID)
cand2match = match(cand2ids,candinfo$CAND_ID)
# Remove those without candidate info (not many)
candna = union(which(is.na(cand1match)),which(is.na(cand2match)))
siginfo4 = data.frame(siginfo3[-candna,],cand1name=candinfo$CAND_NAME[cand1match[-candna]],cand2name=candinfo$CAND_NAME[cand2match[-candna]],cand1aff=candinfo$CAND_PTY_AFFILIATION[cand1match[-candna]],cand2aff=candinfo$CAND_PTY_AFFILIATION[cand2match[-candna]],cand1state=candinfo$CAND_ST[cand1match[-candna]],cand2state=candinfo$CAND_ST[cand2match[-candna]],cand1stat=candinfo$CAND_ICI[cand1match[-candna]],cand2stat=candinfo$CAND_ICI[cand2match[-candna]])
siginfo4f = siginfo4
colnames(siginfo4f)[c(1,2,3,6,7,11,12,13,14,15,16,17,18)] = c("Pvalue","Qvalue","Coef","Cont 1","Cont 2","Name 1","Name 2","Party 1","Party 2","State 1","State 2","Type 1","Type 2")

# Save the permutation pvalue data
save(numperm,jacpvalstore,file="jacpvaldata.rdata")

# Save html table of top 10 results
siginfo4forhtml = siginfo4f[1:10,c(11,12,13,14,3,1,2,6,7,15,16,17,18)]
rownames(siginfo4forhtml) = NULL
sink("top10campaigncorrelations.html")
print(xtable(siginfo4forhtml,display=c("d","s","s","s","s","f","f","f","d","d","s","s","s","s")),type="html")
sink()

# Save html table of top 10 across the aisle results
siginfo4aa = siginfo4f[which(as.character(siginfo4$cand1aff)!=as.character(siginfo4$cand2aff)),]
siginfo4aaforhtml = siginfo4aa[1:10,c(11,12,13,14,3,1,2,6,7,15,16,17,18)]
rownames(siginfo4aaforhtml) = NULL
sink("top10aacampaigncorrelations.html")
print(xtable(siginfo4aaforhtml,display=c("d","s","s","s","s","f","f","f","d","d","s","s","s","s")),type="html")
sink()

# Proportion of incumbents and challengers when correlated overall and when correlated across the aisle
overallinctable = table(siginfo4[,c("cand1stat","cand2stat")])
print(overallinctable/sum(overallinctable))
acrossinctable = table(siginfo4aa[,c("cand1stat","cand2stat")])
print(acrossinctable/sum(acrossinctable))

# Average coefficient of different types of pairings
inconly = which(siginfo4$cand1stat=="I" & siginfo4$cand2stat=="I")
incchal = which((siginfo4$cand1stat=="I" & siginfo4$cand2stat=="C") | (siginfo4$cand1stat=="C" & siginfo4$cand2stat=="I"))
chalonly = which(siginfo4$cand1stat=="C" & siginfo4$cand2stat=="C")
print(median(siginfo4[inconly,3]))
print(median(siginfo4[incchal,3]))
print(median(siginfo4[chalonly,3]))
wilcox.test(siginfo4[inconly,3],siginfo4[incchal,3])

# Save txt table of all significant results
allsigtablewrite = siginfo4f[,c(11,12,13,14,3,1,2,6,7,15,16,17,18,8,9,10)]
colnames(allsigtablewrite)[c(8,9,10)] = c("Tot Cont","ID 1","ID 2")
write.table(allsigtablewrite,file="allsigcampaigncorrelations.txt",quote=FALSE,row.names=FALSE,col.names=TRUE,sep="\t")

# Number of unique campaigns in significant correlations
print(length(unique(c(siginfo4[,9],siginfo4[,10]))))
print(ncol(comtocandcleanmatfb)) # Total possible

# Plot row and column clustered image of contribution data
heatmatchcol = match(colnames(comtocandcleanmatfb),candinfo$CAND_ID)
heatmatchrow = match(rownames(comtocandcleanmatfb),cominfo$CMTE_ID)
comtocandcleanmatfbc = comtocandcleanmatfb[,!is.na(heatmatchcol)]
heatmatchcolc = heatmatchcol[!is.na(heatmatchcol)]
coljac = as.matrix(jaccard(t(comtocandcleanmatfbc)))
rowjac = as.matrix(jaccard(comtocandcleanmatfbc))
matcoldists = as.dist(1-as.matrix(coljac))
mathcolclust = hclust(matcoldists)
matrowdists = as.dist(1-as.matrix(rowjac))
mathrowclust = hclust(matrowdists)
matforheat = comtocandcleanmatfbc[rev(mathrowclust$order),rev(mathcolclust$order)]
affnames = candinfo$CAND_PTY_AFFILIATION[heatmatchcolc[rev(mathcolclust$order)]]
affnames[is.na(affnames)] = "OTHER"
comtypes = cominfo$CMTE_DSGN[heatmatchrow[rev(mathrowclust$order)]]
heatrowcolorpal = brewer.pal(length(unique(comtypes)),"Set1")
heatrowcolors = heatrowcolorpal[as.numeric(as.factor(comtypes))]
affnamesf = as.factor(affnames)
levels(affnamesf)[!(levels(affnamesf)%in%c("DEM","REP"))] = "OTHER"
heatcolcolors = c("grey","#446CCF","#CE2029")[as.numeric(affnamesf)]
png("./plots/binarymat.png",width=1000,height=1500)
# heatmap.2(matforheat,Rowv=FALSE,Colv=FALSE,dendrogram="none",breaks=c(-.5,.5,1.5),col=c("#F1F1F1","blue"),trace="none",labRow="",labCol="",key=FALSE,ColSideColors=heatcolcolors,RowSideColors=heatrowcolors)
# heatmap.2(matforheat,Rowv=FALSE,Colv=FALSE,dendrogram="none",breaks=c(-.5,.5,1.5),col=c("#9D9A96","#3B3C36"),trace="none",labRow="",labCol="",key=FALSE,ColSideColors=heatcolcolors,RowSideColors=heatrowcolors)
# heatmap.2(matforheat,Rowv=FALSE,Colv=FALSE,dendrogram="none",breaks=c(-.5,.5,1.5),col=c("#3B3C36","#FFB347"),trace="none",labRow="",labCol="",key=FALSE,ColSideColors=heatcolcolors,RowSideColors=heatrowcolors)
# heatmap.2(matforheat,Rowv=FALSE,Colv=FALSE,dendrogram="none",breaks=c(-.5,.5,1.5),col=c("#DBD7D2","#3B3C36"),trace="none",labRow="",labCol="",key=FALSE,ColSideColors=heatcolcolors,RowSideColors=heatrowcolors)
heatmap.2(matforheat,Rowv=FALSE,Colv=FALSE,dendrogram="none",breaks=c(-.5,.5,1.5),col=c("#F1F1F1","#3B3C36"),trace="none",labRow="",labCol="",key=FALSE,ColSideColors=heatcolcolors,RowSideColors=heatrowcolors)
dev.off()
png("./plots/binarymatlegend.png",width=1000,height=1000)
plot(1,1,col="white")
legend(x="bottomleft",legend=levels(affnamesf),fill=c("grey","#446CCF","#CE2029"),border="white",bty="n",cex=3)
legend(x="topright",legend=levels(as.factor(comtypes)),fill=heatrowcolorpal,border="white",bty="n",cex=3)
dev.off()

# Close up images of some top contribution associations
indmatforheat = t(matforheat)
heatrowcolorsind = heatcolcolors
heatcolcolorsind = heatrowcolors
curlook = 326
curcompare = c(which(rownames(indmatforheat)==siginfo4aa$cand1id[curlook]),which(rownames(indmatforheat)==siginfo4aa$cand2id[curlook]))
# curcompare = c(which(rownames(indmatforheat)==siginfo4$cand1id[curlook]),which(rownames(indmatforheat)==siginfo4$cand2id[curlook]))
png(paste("./plots/topassociations",paste(rownames(indmatforheat[curcompare,]),collapse="-"),".png",sep=""),width=1000,height=200)
heatmap.2(indmatforheat[curcompare,],Rowv=FALSE,Colv=FALSE,dendrogram="none",breaks=c(-.5,.5,1.5),col=c("#F1F1F1","#3B3C36"),trace="none",labRow="",labCol="",key=FALSE,ColSideColors=heatcolcolorsind,RowSideColors=heatrowcolorsind[curcompare],lwid=c(0.01,5,0.01),lhei=c(0.01,0.89,0.1),keysize=0.1,margins=c(.01,.01),rowsep=1,sepcolor="white")
dev.off()

# Clustered heatmap of jaccard coefficients 
statjac = candinfo$CAND_ICI[heatmatchcolc]
statfjac = as.factor(statjac)
heatcolcolorsstatjac = brewer.pal(length(unique(statfjac)),"Accent")[as.numeric(statfjac)]
coljaclog = log10(coljac+.01)
png("./plots/jacvals.png",width=5000,height=5000)
heatmap.2(coljaclog[rev(mathcolclust$order),rev(mathcolclust$order)],Rowv=FALSE,Colv=FALSE,dendrogram="none",trace="none",labRow="",labCol="",key=FALSE,ColSideColors=heatcolcolors,RowSideColors=heatcolcolorsstatjac[rev(mathcolclust$order)])
dev.off()
png("./plots/jacvalslegend.png",width=1000,height=1000)
plot(1,1,col="white")
legend(x="bottomleft",legend=levels(affnamesf),fill=c("grey","#446CCF","#CE2029"),border="white",bty="n",cex=3)
legend(x="topright",legend=levels(statfjac),fill=brewer.pal(length(unique(statfjac)),"Accent"),border="white",bty="n",cex=3)
dev.off()

