# install package rstudioapi and set file path as current directory
if (!require("rstudioapi")) { install.packages("rstudioapi", dependencies = T) }
library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install missing packages, load packages, sources
source('environment.r')
environment.start()

# load otu table and tree
data.folder <- "data/zotus/infants/"
workload <- otus.load(data.folder, "OTUs-Table.tab", "OTUs-NJTree.tre")
metadata <- read_excel('meta.xlsx')

# extract individuals, timepoints
samples <- row.names(workload$otus)
individuals <- unlist(lapply(samples, function(i){substr(i, start = 2, stop = 4)}))
timepoints <- unlist(lapply(samples, function(i){substr(i, start = 5, stop = 7)}))

# build unique and ordered timepoint list
unique.timepoints <- as.vector(sort(unique(timepoints)))

# cluster data per timepoint
clusterings <- time.series.generate.clusterings(workload$otus, workload$otus.tree)

# generate time series from computed clusters of every timepoint 
time.series <- time.series.generate(clusterings, samples)
write.table(time.series, file = paste(data.folder, 'time-series.txt', sep=''))

# generate transition matrix for selected time points
timepoint.chain <- c("01","03","05","07")
timepoint.chain <- c("07","09","12","24")
timepoint.chain <- c("01","03","05","07","09","12","24")
transition.matrix <- markov.transition.matrix(time.series, timepoint.chain)

plots.network(transition.matrix, timepoint.chain)
heatmap(transition.matrix, Rowv = NA, Colv = NA, scale = 'none', symm = T)

# generate svm classifiers for every timepoint
svm.classifiers <- svm.generate.classifiers(workload$otus, clusterings) 
