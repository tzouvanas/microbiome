# install package rstudioapi and set file path as current directory
if (!require("rstudioapi")) { install.packages("rstudioapi", dependencies = TRUE) }
library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install missing packages, load packages, sources
source('environment.r')
environment.start()

# load otu table and tree
data.folder <- "data/with-students/"
workload <- otus.load(data.folder, "OTUs-Table.tab", "OTUs-NJTree.tre")
otus <- workload$otus
otus.tree <- workload$otus.tree

# extract individuals, timepoints
samples <- row.names(otus)
individuals <- unlist(lapply(samples, function(i){substr(i, start = 2, stop = 4)}))
timepoints <- unlist(lapply(samples, function(i){substr(i, start = 5, stop = 7)}))

# execute analysis per timepoint
clustering.per.timepoint <- lapply(as.vector(sort(unique(timepoints))), function(timepoint){
  otus.of.timepoints <- otus.all.records.for.timepoints(otus, timepoint)
  otus.normalized <- otus.normalize(otus.of.timepoints, 0)
  distances <- time.series.calculate.distances(otus.normalized, otus.tree)
  time.series.cluster.timepoints(otus.normalized, distances, timepoint)
})

# generate time series from computed clusters of every timepoint 
time.series <- time.series.generate(clustering.per.timepoint, samples)
write.table(time.series, file = paste(data.folder, 'time-series.txt', sep=''))

# generate transition matrix for selected time points
timepoint.list <- c("01", "07")
transition.matrix <- markov.transition.matrix(time.series, timepoint.list)
plots.network(transition.matrix, timepoint.list)

#########################################################################################################

timepoint.list <- c("01", "03", "05", "07", "09", "12", "24", "MM")
#timepoint.list <- c("MM")
samples.of.timepoints <- samples[substr(samples, start = 5, stop = 7) %in% timepoint.list]
otus.of.timepoints <- otus.all.records.for.timepoints(otus, timepoint.list)
otus.normalized <- otus.normalize(otus.of.timepoints, 0)
distances.of.timepoints <- time.series.calculate.distances(otus.normalized, otus.tree)

# mds
mds <- cmdscale(distances.of.timepoints, eig = F, k = 2)
plots.timepoints(mds[, 1], mds[, 2], samples.of.timepoints, timepoints[timepoints %in% timepoint.list])

# nmds 
nmds <- metaMDS(distances.of.timepoints, k = 2)

plots.timepoints(nmds$points[,1], nmds$points[,2], samples.of.timepoints, timepoints[timepoints %in% timepoint.list])
plots.samples(nmds$points[,1], nmds$points[,2], samples.of.timepoints, timepoints[timepoints %in% timepoint.list])

plots.individual(nmds$points[,1], nmds$points[,2], samples, timepoints, "011")
plots.individual(nmds$points[,1], nmds$points[,2], samples, timepoints, "016")

clustering <- time.series.cluster.timepoints(otus.normalized, distances.of.timepoints, timepoint.list)
plots.classification(nmds$points[,1], nmds$points[,2], centroid_allocations = clustering$best.clustering_by_ch)



