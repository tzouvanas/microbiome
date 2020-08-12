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
  cluster.timepoint(otus, otus_tree, timepoint)
})

time_series <- generate.time.series(timepoint_clustering_list, samples)
write.table(time_series, file = paste('data/', 'time.series.txt', sep = ''))

# mds nt_analysis
#mds <- cmdscale(timepoint_clustering$distances, eig = F, k = 2)
#otuplots.plot.timepoints(mds[, 1], mds[, 2], samples, timepoints)

# nmds 
#nmds <- metaMDS(timepoint_clustering$distances, k = 2)
#otuplots.plot.timepoints(nmds$points[,1], nmds$points[,2], samples, timepoints)






