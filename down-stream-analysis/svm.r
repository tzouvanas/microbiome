# train an svm classifier for every timepoint clustering
svm.generate.classifiers <- function(otus, clusterings, percentage=0.8, kernel='linear'){

  # execute analysis per timepoint
  svms <- lapply(clusterings, function(clustering){
    
    # filter otus with timepoint
    otus.of.timepoint <- otus.all.records.for.timepoints(otus, clustering$timepoint)
    
    # normalize otus
    otus.normalized <- otus.normalize(otus.of.timepoint, 0)
    
    # train a classifier for every timepoint  
    svm.classifier <- svm.train.classifier(otus.normalized, clustering$best.clustering_by_ch, percentage, kernel)  
    
    result <- list('svm' = svm.classifier, 'timepoint' = clustering$timepoint)
    
    return(result)
  })
  
  return(svms)
}


svm.train.classifier <- function(data, classification, percentage, kernel){
  
  unique.classification.values <- unique(classification)
  
  training.data.set <- array(dim=c(0,ncol(data)))
  training.classification.set <- array(dim=c(0,ncol(data)))
  
  validation.data.set <- array(dim=c(0,ncol(data)))
  validation.classification.set <- array(dim=c(0,ncol(data)))
  
  # for every classification value create a training and validation set
  # merge the results and traing the SVM
  for(classification.value in unique.classification.values){
  
    classification.value.index <- classification == classification.value
    
    data.for.specific.value <- data[classification.value.index, ]
    classification.for.specific.value <- classification[classification.value.index]
    
    # split data into training and test set
    nrOfTrainingSamples <- round(percentage*nrow(data.for.specific.value), 0)
    training.indeces <- sample(1:nrow(data.for.specific.value), nrOfTrainingSamples, replace = F)
    
    training.data.set.for.specific.value <- data.for.specific.value[training.indeces,]
    training.classification.set.for.specific.value <- classification.for.specific.value[training.indeces]
    
    validation.data.set.for.specific.value <- data.for.specific.value[-training.indeces,]
    validation.classification.set.for.specific.value <- classification.for.specific.value[-training.indeces]   
    
    training.data.set <- rbind(training.data.set, training.data.set.for.specific.value)
    training.classification.set <- c(training.classification.set, training.classification.set.for.specific.value)
    
    validation.data.set <- rbind(validation.data.set, validation.data.set.for.specific.value)
    validation.classification.set <- c(validation.classification.set, validation.classification.set.for.specific.value)
  }

  # train SVM with training set
  classifier <- svm(x = training.data.set, 
                    y = training.classification.set, 
                    type = "C-classification",
                    kernel = kernel, 
                    cost = 1, 
                    scale = F)
  
  # validate training
  prediction.on.validation.set <- svm.predict(classifier, validation.data.set)
  
  confusion.matrix <- table(prediction.on.validation.set, validation.classification.set)
  
  result <- list('classifier' = classifier,
                 'nrOfTrainingSamples' = nrOfTrainingSamples,
                 'confusion.matrix'= confusion.matrix)
  
  return(result)
}

svm.predict <- function(classifier, data){
  prediction <- predict(classifier, data)
  return(prediction)
}