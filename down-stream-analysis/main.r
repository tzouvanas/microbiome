# cleanup console
cat("\014")  

# cleanup environment variables
rm(list=ls(all=TRUE))

# read otu table
otu_filename <- 'otu-table.txt'
otu_relative_path <- paste("./data/", otu_filename, sep = '')
otu_table <- t(read.delim2(otu_relative_path, header=T, sep="\t", row.names = 1))

# extract sample, time-slots
sample_names <- rownames(otu_table)
samples <- lapply(sample_names, function(i){substr(i, start = 2, stop = 4)})
timeSlots <- lapply(sample_names, function(i){substr(i, start = 5, stop = 7)})

# classify samples with k-medians
source('k-medians.r')
centroid_allocations <- as.vector(kmedians(otu_table, nr_of_clusters = 3, init_method = 'k-means'))

# run mds on distance data, projecting to 2 dimensions
distances = as.matrix(dist(otu_table, method = "manhattan"))
scaled_distances <- cmdscale(distances, eig = F, k = 2)
x <- scaled_distances[, 1]
y <- scaled_distances[, 2]

library(plotly)
plot_ly(data = data.frame(sample_names), x = x, y = y,
        color = centroid_allocations, colors = c('red', 'green', 'blue'),
        type = "scatter",
        mode = "markers")

