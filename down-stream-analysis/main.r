# cleanup console
cat("\014")  

# cleanup environment variables
rm(list=ls(all=TRUE))

# read otu table
otu_filename = 'test-otu-table.txt'
otu_table <- read.delim2(paste("./data/", otu_filename, sep = ''), header=T, sep="\t", row.names = 1)
otu_matrix <- as.matrix(otu_table)

# extract sample, time-slots
row_names <- row.names(otu_table)
samples <- lapply(row_names, function(i){substr(i, start = 2, stop = 4)})
timeSlots <- lapply(row_names, function(i){substr(i, start = 5, stop = 7)})

# calculate distances between samples
distance_matrix <- dist(t(otu_matrix), method = 'manhattan', diag = T, upper = T)

source('k-medians.r')
centroid_allocations <- kmedians(t(otu_matrix), 3)
plot(centroid_allocations)
