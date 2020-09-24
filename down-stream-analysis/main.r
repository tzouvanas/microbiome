# install package rstudioapi and set file path as current directory
if (!require("rstudioapi")) { install.packages("rstudioapi", dependencies = TRUE) }
library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install missing packages, load packages, sources
source('environment.r')
environment.start()

# load otu table and tree
otus <- t(read.delim2(paste("data/", 'OTUs-Table.tab', sep = ''), header=T, sep="\t", row.names = 1))
otus.contains.taxonomy.row <- tail(rownames(otus), n=1) == "taxonomy"
if (otus.contains.taxonomy.row) {otus <- otus[1:nrow(otus)-1,]}
mode(otus) <- "integer"
otus <- otus[ order(row.names(otus)), ]
otus.tree <- read.tree(paste("data/", 'OTUs-MLTree.tre', sep = ''))

# extract individuals, timepoints
samples <- row.names(otus)
individuals <- unlist(lapply(samples, function(i){substr(i, start = 2, stop = 4)}))
timepoints <- unlist(lapply(samples, function(i){substr(i, start = 5, stop = 7)}))

# execute analysis per timepoint
type.of.distance <- "euclidian"
clustering.per.timepoint <- lapply(as.vector(sort(unique(timepoints))), function(timepoint){
  time.series.cluster.timepoint(otus, otus.tree, timepoint, type.of.distance)
})

time.series <- time.series.generate(clustering.per.timepoint, samples)
write.table(time.series, file = paste('data/', 'time.series.txt', sep=''))

time.point.list <- c("01", "24")
transition.matrix <- markov.transition.matrix(time.series, time.point.list)
dtmcA <- new("markovchain", transitionMatrix = transition.matrix, states = colnames(transition.matrix), name = "A") 
plot(dtmcA)

#########################################################################################################
timepoint <- NULL
clustering.of.timepoint <- time.series.cluster.timepoint(otus, otus.tree, timepoint, type.of.distance)

samples.of.timepoint <- samples
if (!is.null(timepoint)) samples.of.timepoint <- samples[substr(samples, start = 5, stop = 7) == timepoint]

timepoints.of.timepoint <- timepoints
if (!is.null(timepoint)) timepoints.of.timepoint <- timepoint

# mds
mds <- cmdscale(clustering.of.timepoint$distances, eig = F, k = 2)
plots.timepoints(mds[, 1], mds[, 2], samples.of.timepoint, timepoints.of.timepoint)
plots.samples(mds[, 1], mds[, 2], samples.of.timepoint, timepoints.of.timepoint)
plots.classification(mds[, 1], mds[, 2], clustering.of.timepoint$best.clustering_by_slh)

# nmds 
nmds <- metaMDS(clustering.of.timepoint$distances, k = 2)
plots.timepoints(nmds$points[,1], nmds$points[,2], samples.of.timepoint, timepoints.of.timepoint)
plots.samples(nmds$points[,1], nmds$points[,2], samples.of.timepoint, timepoints.of.timepoint)
plots.classification(nmds$points[,1], nmds$points[,2], clustering.of.timepoint$best.clustering_by_slh)
plots.individual(nmds$points[,1], nmds$points[,2], samples.of.timepoint, timepoints.of.timepoint, "016")


