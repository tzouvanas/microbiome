time.series.calculate.distances <- function(otus.normalized, otus.tree, distance.type = "unifrac"){
 
  if (is.null(timepoints) || is.na(timepoints)) {
    stop("time.series.cluster.timepoint() function called with empty timepoints")
    return(NA)
  }

  distance <- NULL
  if (distance.type == "unifrac"){
    unifracs <- GUniFrac(otus.normalized, midpoint(otus.tree), alpha = c(0.0, 0.5, 1.0))$unifracs
    distance <- as.dist(unifracs[, , "d_0.5"])
  }
  else if (distance.type == "euclidian") {
    distance <- dist(otus.normalized,  method = 'euclidian', upper = T)
  }
  else if (distance.type == "manhattan") {
    distance <- dist(otus.normalized,  method = 'manhattan', upper = T)
  } 
  
  if (is.null(distance) || is.na(distance)) {
    stop("time.series.cluster.timepoint() function called with invalid distance type")
    return(NA)
  }
  
  return(distance)
}

time.series.cluster.timepoints <- function(otus.normalized, distances.of.timepoints, timepoint){
  
  # cluster distances using k-medoids
  max_nr_of_clusters <- nrow(otus.normalized) - 1
  clustering_instances <- lapply(2:max_nr_of_clusters, function(nr_of_clusters){
    kmedoids <- pam(distances.of.timepoints, nr_of_clusters, diss = T)
    calinski_harabasz_index <- calinhara(otus.normalized, kmedoids$clustering, nr_of_clusters)
    silhouette_index <- kmedoids$silinfo$avg.width
    list('clustering' = kmedoids$clustering, 'ch_benchmark' = calinski_harabasz_index, 'slh_benchmark' = silhouette_index)
  })
  
  ch_benchmark_list <- unlist(lapply(clustering_instances, function(instance){instance$ch_benchmark}))
  slh_benchmark_list <- unlist(lapply(clustering_instances, function(instance){instance$slh_benchmark}))
  
  best_clustering_index_by_slh <- which(slh_benchmark_list == max(slh_benchmark_list))
  maxL <- (length(ch_benchmark_list) / 2) + 1
  best_clustering_index_by_ch <- which(ch_benchmark_list == max(ch_benchmark_list[1:maxL]))
  
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
                 'timepoint' = timepoint)
  
  return (result)
}

time.series.generate <- function(clustering.per.timepoint, samples){
  
  individuals <- unlist(lapply(samples, function(i){substr(i, start = 2, stop = 4)}))
  timepoints <- unlist(lapply(samples, function(i){substr(i, start = 5, stop = 7)}))
  
  time_series <- matrix(nrow = length(unique(individuals)), ncol = length(unique(timepoints)))
  row.names(time_series) <- unique(individuals)
  colnames(time_series) <- unique(timepoints)
  
  for (timepoint_clustering in clustering.per.timepoint)
  {
    for (individual in unique(individuals))
    {
      timepoint <- timepoint_clustering$timepoint
      sample = paste('X', individual, timepoint, sep = '')
      time_series[individual, timepoint] = timepoint_clustering$best.clustering_by_ch[sample]
    }
  }
  
  return(time_series)
}


