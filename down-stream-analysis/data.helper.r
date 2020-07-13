all.records.of.sample <- function(X, sample_id){
  row_names <- row.names(X)
  indeces <- which(startsWith(row_names, paste("X", sample_id, sep = '')))
  return(X[indeces, ])
}

all.records.of.time.slot <- function(X, time_slot){
  row_names <- row.names(X)
  indeces <- which(endsWith(row_names, time_slot))
  return(X[indeces, ])
}