otus.normalize <- function(otus, otu_threashold){
  
  # calculate otu percentages per sample
  otus_sums_by_sample <- rowSums(otus)
  otus_percentages <- otus / otus_sums_by_sample
  
  # filter otus. if an otus is below threashold for all samples, exclude it
  otus_to_keep <- colSums(otus_percentages > otu_threashold) > 0
  otus_percentages_filtered <- otus_percentages[, otus_to_keep]
  
  # normalize otus
  otus_norm <- max(rowSums(otus)) * otus_percentages_filtered
  mode(otus_norm) <- "integer"
  
  return(otus_norm)
}