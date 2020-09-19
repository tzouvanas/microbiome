# install package rstudioapi and set file path as current directory
if (!require("rstudioapi")) { install.packages("rstudioapi", dependencies = TRUE) }
library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install missing packages, load packages, sources
source('environment.r')
environment.start()

# load otu table and tree
otus <- t(read.delim2(paste("data/", 'otu-table.txt', sep = ''), header=T, sep="\t", row.names = 1))
otus.tree <- read.tree(paste("data/", 'otu-tree.txt', sep = ''))

# extract individuals, timepoints
samples <- row.names(otus)
individuals <- unlist(lapply(samples, function(i){substr(i, start = 2, stop = 4)}))
timepoints <- unlist(lapply(samples, function(i){substr(i, start = 5, stop = 7)}))

# execute analysis per timepoint
clustering.per.timepoint <- lapply(as.vector(sort(unique(timepoints))), function(timepoint){
  time.series.cluster.timepoint(otus, otus.tree, timepoint, "unifrac")
})

time.series <- time.series.generate(clustering.per.timepoint, samples)
write.table(time.series, file = paste('data/', 'time.series.txt', sep=''))

time.point.list <- c("03", "07", "24")
transition.matrix <- markov.transition.matrix(time.series, time.point.list)
dtmcA <- new("markovchain", transitionMatrix = transition.matrix, states = colnames(transition.matrix), name = "A") 
plot(dtmcA)

#########################################################################################################
timepoint <- "01"
distance_type <- "unifrac"
clustering.of.timepoint <- time.series.cluster.timepoint(otus, otus.tree, timepoint, distance_type)

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



