otus.all.records.for.sample <- function(X, sample_id){
  row_names <- row.names(X)
  indeces <- which(startsWith(row_names, paste("X", sample_id, sep = '')))
  return(X[indeces, ])
}

otus.all.records.for.timepoint <- function(otu_table, timepoint){
  row_names <- row.names(otu_table)
  indeces <- which(endsWith(row_names, timepoint))
  return(otu_table[indeces, ])
}