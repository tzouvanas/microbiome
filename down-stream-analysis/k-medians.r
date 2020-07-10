source('common.r')

kmedians.medians.of.columns <- function(X){
  
  medians = list()
  nr_of_columns <- ncol(X)

  for (i in 1:nr_of_columns){
    median_of_column_i = median(X[,i], na.rm = F)
    medians[[i]] = median_of_column_i
  }
  
  return(unlist(medians))
}
  
kmedians.init.centroids <- function(nr_of_dimensions, nr_of_centroids){
  centroids <- list()
  for (i in 1:nr_of_centroids)
  {
    centroids[[i]] <- floor(runif(nr_of_dimensions, min=0, max=500))
  }
  return (centroids)
}

kmedians.nearest.centroid <- function(centroids, data_vector){
  
  nr_of_centroids = length(centroids)
  distance_from_centroids = list()
  
  for (k in 1:nr_of_centroids){
    centroid = centroids[[k]]
    distance_from_centroids[[k]] = manhattan.distance(data_vector, centroid)
  }
  
  min_distance = min(unlist(distance_from_centroids))
  centroid_index = which.min(unlist(distance_from_centroids))
  
  return (list(centroid_index, min_distance))
}

kmedians.allocate.to.centroids <- function(X, centroids){
  
  nr_of_items = nrow(X)
  centroid_allocations = matrix(nrow = nr_of_items, ncol = 1)
  
  for (i in 1:nr_of_items){
    data_vector <- X[i,]
    nearest_centroid <- kmedians.nearest.centroid(centroids, data_vector)
    centroid_allocations[i,1] = nearest_centroid[[1]]
  }
  
  return(centroid_allocations)
}

kmedians.update.centroids <- function(X, centroids, centroid_allocations){
  
  new_centroids <- list()
  nr_of_centroids <- length(centroids)
  
  for (k in 1:nr_of_centroids){
    item_indexes_allocated_to_centroid <- which(centroid_allocations[, 1] == k)
    items_allocated_to_centroid <- as.matrix(X[item_indexes_allocated_to_centroid, ])

    if (nrow(items_allocated_to_centroid) == 0){
      new_centroids[[k]] <- integer(ncol(X))
    }
    else{
      items_medians <- kmedians.medians.of.columns(items_allocated_to_centroid)
      new_centroids[[k]] <- items_medians
    }
  }
  
  return(new_centroids)
}

kmedians <- function(X, nr_of_clusters, max_iterations = 100){
  
  centroids <- kmedians.init.centroids(nr_of_dimensions = ncol(X), nr_of_centroids = nr_of_clusters)

  for (i in 1:max_iterations){

    centroid_allocations = kmedians.allocate.to.centroids(X, centroids)
    new_centroids <- kmedians.update.centroids(X, centroids, centroid_allocations)
    
    no_change_in_centroids = vectors.are.equal(unlist(centroids), unlist(new_centroids))
    if (no_change_in_centroids) { break }

    centroids = new_centroids
  }
  
  return(centroid_allocations)
}