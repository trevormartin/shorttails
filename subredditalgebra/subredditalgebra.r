#######################################
#
# Program to analyze distance between
# Reddit subreddits using the cooccurrence
# of commentors across subreddits
# (similar analysis to word2vec but on subreddits)
# Prep code
# See shorttails.io
#
#######################################

library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(gplots)
#library(scales)
library(xtable)
library(lubridate)
library(lsa)
library(Rtsne)

##### Part 0: Formatted and processed data in BigQuery

## Creating list of number of users in each subreddit: 
#SELECT subreddit, authors, DENSE_RANK() OVER (ORDER BY authors DESC) AS rank_authors
#FROM (SELECT subreddit, SUM(1) as authors
#      FROM (SELECT subreddit, author, COUNT(1) as cnt 
#          FROM [fh-bigquery:reddit_comments.all_starting_201501]
#          WHERE author NOT IN (SELECT author FROM [fh-bigquery:reddit_comments.bots_201505])
#          GROUP BY subreddit, author HAVING cnt > 0)
#      GROUP BY subreddit) t
#ORDER BY authors DESC;

## Creating list of number of users who authored at least 10 posts in pairs of subreddits: 
#SELECT t1.subreddit, t2.subreddit, SUM(1) as NumOverlaps
#FROM (SELECT subreddit, author, COUNT(1) as cnt 
#      FROM [fh-bigquery:reddit_comments.all_starting_201501]
#      WHERE author NOT IN (SELECT author FROM [fh-bigquery:reddit_comments.bots_201505])
#      AND subreddit IN (SELECT subreddit FROM [subreddit-vectors:subredditoverlaps.subr_rank_all_starting_201501]
#        WHERE rank_authors>200 AND rank_authors<2201)
#      GROUP BY subreddit, author HAVING cnt > 10) t1
#JOIN (SELECT subreddit, author, COUNT(1) as cnt 
#      FROM [fh-bigquery:reddit_comments.all_starting_201501]
#      WHERE author NOT IN (SELECT author FROM [fh-bigquery:reddit_comments.bots_201505])
#      GROUP BY subreddit, author HAVING cnt > 10) t2
#ON t1.author=t2.author
#WHERE t1.subreddit!=t2.subreddit
#GROUP BY t1.subreddit, t2.subreddit

##### Part 1: Load in the data

rawsubredditvecs = read.table("all_starting_2015_01_overlaps_top2200_no200_10com_allrank_mod.csv",header=TRUE,sep=",")

##### Part 2: Format and clean data for analysis

castsubredditvecs = dcast(rawsubredditvecs,t1_subreddit~t2_subreddit,FUN="identity",fill=0)
subredditvecst = as.matrix(castsubredditvecs[,-1])
rownames(subredditvecst) = castsubredditvecs[,1]
subredditvecs = t(subredditvecst)
subredditvecssums = apply(subredditvecs,1,sum)
subredditvecsnorm = sweep(subredditvecs,1,subredditvecssums,"/")
subredditvecshellinger=sqrt(subredditvecsnorm)/sqrt(2) # hellinger version
subredditvecssumscontext = apply(subredditvecs,2,sum)
contextprobs = subredditvecssumscontext/sum(subredditvecssumscontext)
subredditvecspmi = log(sweep(subredditvecsnorm,2,contextprobs,"/")) # pmi version
subredditvecsppmi = subredditvecspmi
subredditvecsppmi[subredditvecspmi<0] = 0 # ppmi version
res.pca = prcomp(subredditvecsppmi) # takes ~30min
subredditvecspca = res.pca$x[,11:2001] # take subreddits mapped onto subset of PCs
scalar1 <- function(x) {x / sqrt(sum(x^2))} # function to normalize vectors
subredditvecspcanorm = t(apply(subredditvecspca,1,scalar1))
entropypreph = subredditvecsnorm*log2(subredditvecsnorm)/log2(ncol(subredditvecsnorm))
entropyprep = entropypreph; entropyprep[!is.finite(entropypreph)] = 0
entropyrownorms = 1+apply(entropyprep,1,sum)
idfrownorms = log2(ncol(subredditvecs)/(1+apply((subredditvecs!=0),1,sum)))
subredditvecslogentropy = log2(sweep(subredditvecs,1,entropyrownorms,"*")+1)
subredditvecslogidf = log2(sweep(subredditvecs,1,idfrownorms,"*")+1)

##### Part 3: Analysis of subreddit similarities

## Looking at which subreddits are closest to each other (and combinations of subreddits)
cursubmat = subredditvecsppmi # works really well
cursubmat = subredditvecspca # 
cursubmat = subredditvecsnorm # 
cursubmat = subredditvecslogentropy # 
cursubs = c("dataisbeautiful")
curops = c("+","+")
findrelsubreddit <- function(cursubs,curops,numret=20) {
    curvec = 0
    for(i in 1:length(cursubs)) {
	    curvec = ifelse(curops[i]=="+",list(curvec + cursubmat[which(rownames(cursubmat)==cursubs[i]),]),list(curvec - cursubmat[which(rownames(cursubmat)==cursubs[i]),]))[[1]]
    }
    curclosesubs = cosine(x=curvec,y=t(cursubmat))
    curclosesubso = order(curclosesubs,decreasing=TRUE)
#return(head(cbind(rownames(cursubmat)[curclosesubso],curclosesubs[curclosesubso]),numret))
return(head(curclosesubs[curclosesubso],numret))
}
findrelsubreddit(cursubs,curops)

## t-SNE plot of all subreddits
set.seed(123) # For reproducibility
#curtsnemat = subredditvecspca[order(subredditvecssums,decreasing=TRUE)[1:500],]
#curtsnemat = subredditvecsppmi[order(subredditvecssums,decreasing=TRUE)[1:1000],]
curtsnemat = subredditvecsppmi[order(subredditvecssums,decreasing=TRUE),]
#rtsne_out = Rtsne(curtsnemat,dims=2,check_duplicates=FALSE,perplexity=30,verbose=TRUE,pca=FALSE)
rtsne_out = Rtsne(curtsnemat,dims=2,check_duplicates=FALSE,perplexity=50,verbose=TRUE,pca=FALSE)
png("testtsneplot.png", width=20, height=20,units="in",res=350)
plot(rtsne_out$Y, t='n', main="BarnesHutSNE")
text(rtsne_out$Y, labels=rownames(curtsnemat),cex=.5)
dev.off()
png("testtsneplot.png", width=40, height=40,units="in",res=350)
plot(rtsne_out$Y, t='n', main="BarnesHutSNE")
points(rtsne_out$Y)
dev.off()
# k-means clustering for color
numkgroups = 20
kmeans_out = kmeans(curtsnemat,centers=numkgroups)
kmeanscols = c("#442637", "#74d958", "#6242c5", "#d0d149", "#be4cd5", "#619141", "#c44398", "#6fcfaf", "#d95035", "#667ecc", "#c78a41", "#512f75", "#c7ca94", "#ce4a6d", "#90bdd7", "#843a2e", "#c589c9", "#515631", "#c49295", "#466675")
png("tsneplotlarge.png", width=60, height=60,units="in",res=350)
plot(rtsne_out$Y, t='n', main="Reddit Subreddits - BarnesHutSNE")
text(rtsne_out$Y, labels=rownames(curtsnemat),cex=.25,col=kmeanscols[kmeans_out$cluster])
dev.off()

##### Part 5: Save data for interactive tools

## t-SNE data
tsnecoords = rtsne_out$Y
rownames(tsnecoords) = rownames(curtsnemat)
tsnesizes = subredditvecssums[order(subredditvecssums,decreasing=TRUE)]
tsnecolors = kmeanscols
tsneclusters = kmeans_out$cluster
save(tsnecoords,tsnesizes,tsnecolors,tsneclusters,file="tsnedata.rdata")
## Subreddit similarity matrix
subsimmat = subredditvecsppmi
save(subsimmat,file="subredsimdata.rdata")






test=cosine(x=cursubmat[which(rownames(cursubmat)=="dataisbeautiful"),],y=t(cursubmat))
test=cosine(x=(cursubmat[which(rownames(cursubmat)=="Sacramento"),]+cursubmat[which(rownames(cursubmat)=="sanfrancisco"),]),y=t(cursubmat))

