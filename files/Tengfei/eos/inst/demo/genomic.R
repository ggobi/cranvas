library(qtpaint)
library(IRanges)
options(warn=0)
## options(error=recover)

sourceDir <- function(path, trace = TRUE, ...) {
         for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
            if(trace) cat(nm,":")           
            source(file.path(path, nm), ...)
            if(trace) cat("\n")
         }
      }

sourceDir('~/Prolang/git/cranvas/Tengfei/eos/R')

load('~/Prolang/git/cranvas/Tengfei/eos/data/cytobands.rda')
obj <- cytobands[[1]]
lst <- by(obj,obj$chr,function(x) data.frame(chr=unique(x$chr),start=min(x$start),end=max(x$end)))

mydf <- do.call('rbind',lst)

## ord <- apply(mydf,1,function(x){
##   temp <- substr(x[['chr']],4,nchar(x[['chr']]))
##   if(temp !%in% c('X','Y')){
##     temp <- as.numeric(temp)
##   }
## })

library("humanStemCell")
data(fhesc)
library("genefilter")
filtFhesc <- nsFilter(fhesc)[[1]]
library("limma")
design <- model.matrix(~filtFhesc$Diff)
hesclim <- lmFit(filtFhesc, design)
hesceb <- eBayes(hesclim)
tab <- topTable(hesceb, coef = 2, adjust.method = "BH",
n = 7676)
tab2 <- tab[(tab$logFC > 1) & (tab$adj.P.Val < 0.01),
]
affyIDs <- tab2$ID
library("microRNA")
data(hsTargets)
library("hgu133plus2.db")
entrezIDs <- mappedRkeys(hgu133plus2ENTREZID[affyIDs])
library("org.Hs.eg.db")
mappedEntrezIDs <- entrezIDs[entrezIDs %in% mappedkeys(org.Hs.egENSEMBLTRANS)]
ensemblIDs <- mappedRkeys(org.Hs.egENSEMBLTRANS[mappedEntrezIDs])
targetMatches <- match(ensemblIDs, hsTargets$target,
0)
targets <- hsTargets[targetMatches, ]
library(IRanges)
targetRanges <- IRanges(targets$start, targets$end)
library(rtracklayer)
targetTrack <- GenomicData(targetRanges, targets[, c("strand",
"name", "target")], value=c(5,6),chrom = paste("chr", targets$chrom,
sep = ""), genome = "hg19")

targetTrack


ir <- IRanges(start=mydf$start,end=mydf$end)
ird <- RangedData(ir,space=mydf$chr)
idx <- order(width(ird))
ird <- ird[rev(idx)]

ir2 <- IRanges(start=c(1,10,20),width=1)
xtemp <- sapply(end(ird),function(end) sample(1:(end/100000),30)*100000)
ir3 <- IRanges(start=as.numeric(xtemp),width=1)
ird3 <- RangedData(ir3,space=rep(space(ird),30),value=sample(5:100,30*24,replace=TRUE))

.TYPES <- c('sector','segment','text','line','step','point')
## no step yet

e1 <- EOSTrack(ird,type='sector')
e3 <- EOSTrack(ird,type='text')
e4 <- EOSTrack(targetTrack,type='segment')
e5 <- EOSTrack(ird3,type='point')
e6 <- EOSTrack(ird3,type='line')
eosview <- EOSView(list(e4,e1,e5,e6,e3),scale=max(end(ird)),globalmap=ird)

eosplot(eosview)

library(IRanges)
ranges <- IRanges(c(1,2,3),c(4,5,6))
filter <- c(1L,0L,1L)
score <- c(10L,2L,NA)
rd <- RangedData()
rd <- RangedData(ranges)
rd <- RangedData(ranges,score)
rd
