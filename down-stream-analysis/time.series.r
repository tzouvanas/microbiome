time.series.cluster.timepoint <- function(otus, otus_tree, timepoint, distance_type){

  timepoint_otus <- otus
  if (!is.null(timepoint)){
    timepoint_otus <- otus.all.records.for.timepoint(otus, timepoint)
  }

  # filter out low abandancies and normalize otus
  otus_threashold <- 0.025
  otus_norm <- otus.normalize(timepoint_otus, otus_threashold)

  # distances
  unifracs <- GUniFrac(otus_norm, midpoint(otus_tree), alpha = c(0.0, 0.5, 1.0))$unifracs
  unifract_dist <- as.dist(unifracs[, , "d_0.5"])
  manhattan_dist <- dist(otus_norm,  method = 'manhattan', upper = T)
  euclidian_dist <- dist(otus_norm,  method = 'euclidian', upper = T)
  
  distance <- unifract_dist
  if (distance_type == "euclidian") {distance <- euclidian_dist}
  if (distance_type == "manhattan") {distance <- manhattan_dist}
  
  # cluster distances using k-medoids
  max_nr_of_clusters <- nrow(otus_norm) - 2
  clustering_instances <- lapply(2:max_nr_of_clusters, function(nr_of_clusters){
    kmedoids <- pam(distance, nr_of_clusters, diss = T)
    calinski_harabasz_index <- calinhara(otus_norm, kmedoids$clustering, nr_of_clusters)
    silhouette_index <- kmedoids$silinfo$avg.width
    list('clustering' = kmedoids$clustering, 'ch_benchmark' = calinski_harabasz_index, 'slh_benchmark' = silhouette_index)
  })
  
  ch_benchmark_list <- unlist(lapply(clustering_instances, function(instance){instance$ch_benchmark}))
  slh_benchmark_list <- unlist(lapply(clustering_instances, function(instance){instance$slh_benchmark}))
  
  best_clustering_index_by_slh <- which(slh_benchmark_list == max(slh_benchmark_list))
  best_clustering_index_by_ch <- which(ch_benchmark_list == max(ch_benchmark_list))
  
  best_nr_of_clusters_by_slh <- best_clustering_index_by_slh + 1
  best_nr_of_clusters_by_ch <- best_clustering_index_by_ch + 1
  
  best_clustering_by_slh <- clustering_instances[[best_clustering_index_by_slh]]$clustering
  best_clustering_by_ch <- clustering_instances[[best_clustering_index_by_ch]]$clustering

  # build result
  result <- list('best.clustering_by_slh' = best_clustering_by_slh, 
                 'best.clustering_by_ch' = best_clustering_by_ch,
                 'best.nr.of.clusters_by_slh' = best_nr_of_clusters_by_slh,
                 'best.nr.of.clusters_by_ch' = best_nr_of_clusters_by_ch,
                 'ch_benchmark_list' = ch_benchmark_list,
                 'slh_benchmark_list' = slh_benchmark_list,
                 'distances' = distance,
                 'timepoint' = timepoint)
  
  return (result)
}

time.series.generate <- function(timepoint_clustering_list, samples){
  
  individuals <- unlist(lapply(samples, function(i){substr(i, start = 2, stop = 4)}))
  timepoints <- unlist(lapply(samples, function(i){substr(i, start = 5, stop = 7)}))
  
  time_series <- matrix(nrow = length(unique(individuals)), ncol = length(unique(timepoints)))
  row.names(time_series) <- unique(individuals)
  colnames(time_series) <- unique(timepoints)
  
  for (timepoint_clustering in timepoint_clustering_list)
  {
    for (individual in unique(individuals))
    {
      timepoint <- timepoint_clustering$timepoint
      sample = paste('X', individual, timepoint, sep = '')
      time_series[individual, timepoint] = timepoint_clustering$best.clustering_by_slh[sample]
    }
  }
  
  return(time_series)
}


