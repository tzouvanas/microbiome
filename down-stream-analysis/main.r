# cleanup console
cat("\014")  

# cleanup environment variables
rm(list=ls(all=TRUE))

# current directory
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read otu table
otu_filename <- 'otu-table.txt'
otu_relative_path <- paste("./data/", otu_filename, sep = '')
otu_table <- read.delim2(otu_relative_path, header=T, sep="\t", row.names = 1)
sample_table <- t(otu_table)

# extract sample, months
sample_names <- rownames(sample_table)
samples <- unlist(lapply(sample_names, function(i){substr(i, start = 2, stop = 4)}))
months <- unlist(lapply(sample_names, function(i){substr(i, start = 5, stop = 7)}))

# classify samples with k-medians
source('k-medians.r')
k_medians_allocations <- as.vector(kmedians(sample_table, nr_of_clusters = 3, init_method = ''))
plot(k_medians_allocations)

# run mds on distance data, projecting to 2 dimensions
distances = as.matrix(vegdist(sample_table, method = "bray"))
scaled_distances <- cmdscale(distances, eig = F, k = 2)
x <- scaled_distances[, 1]
y <- scaled_distances[, 2]

# plot scaled distances
library(plotly)
plot_ly(data = data.frame(sample_names), x = x, y = y,
        #color = centroid_allocations, colors = c('red', 'green', 'blue'),
        color = months, colors = c('red', 'green', 'blue', 'orange', 'black', 'purple', 'cyan'),
        type = "scatter",
        mode = "markers")
