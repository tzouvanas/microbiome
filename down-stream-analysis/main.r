# install package rstudioapi and set file path as current directory
if (!require("rstudioapi")) { install.packages("rstudioapi", dependencies = TRUE) }
library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install missing packages, load packages, sources
source('environment.r')
environment.start()

# load otu table and tree
otus <- t(read.delim2(paste("data/", 'otu-table.txt', sep = ''), header=T, sep="\t", row.names = 1))
otus_tree <- read.tree(paste("data/", 'otu-tree.txt', sep = ''))

# extract individuals, timepoints
samples <- row.names(otus)
individuals <- unlist(lapply(samples, function(i){substr(i, start = 2, stop = 4)}))
timepoints <- unlist(lapply(samples, function(i){substr(i, start = 5, stop = 7)}))

# execute analysis per timepoint
timepoint_clustering_list <- lapply(as.vector(sort(unique(timepoints))), function(timepoint){
  timeseries.cluster.timepoint(otus, otus_tree, timepoint, "unifrac")
})

time.series <- timeseries.generate(timepoint_clustering_list, samples)
write.table(timeseries, file = paste('data/', 'time.series.txt'), sep = '')

time.point.list <- NULL
transition.matrix <- markov.transition.matrix(time.series, time.point.list)


#########################################################################################################
timepoint <- NULL
distance_type <- "unifrac"
timepoint_clustering <- time.series.cluster.timepoint(otus, otus_tree, timepoint, distance_type)

timepoint_samples <- samples
if (!is.null(timepoint)) timepoint_samples <- samples[substr(samples, start = 5, stop = 7) == timepoint]

timepoint_timepoints <- timepoints
if (!is.null(timepoint)) timepoint_timepoints <- NULL

# mds
mds <- cmdscale(timepoint_clustering$distances, eig = F, k = 2)
otuplots.plot.timepoints(mds[, 1], mds[, 2], timepoint_samples, timepoint_timepoints)

# nmds 
nmds <- metaMDS(timepoint_clustering$distances, k = 2)
otuplots.plot.timepoints(nmds$points[,1], nmds$points[,2], timepoint_samples, timepoint_timepoints)



