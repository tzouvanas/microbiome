# train an svm classifier for every timepoint clustering
svm.generate.classifiers <- function(otus, clusterings, percentage=0.6){

  # execute analysis per timepoint
  svms <- lapply(clusterings, function(clustering){
    
    # filter otus with timepoint
    otus.of.timepoint <- otus.all.records.for.timepoints(otus, clustering$timepoint)
    
    # normalize otus
    otus.normalized <- otus.normalize(otus.of.timepoint, 0)
    
    # train a classifier for every timepoint  
    svm.classifier <- svm.train.classifier(otus.normalized, clustering$best.clustering_by_ch, percentage)  
    
    result <- list('svm' = svm.classifier, 'timepoint' = clustering$timepoint)
    
    return(result)
  })
  
  return(svms)
}


svm.train.classifier <- function(data, classification, percentage, kernel='linear'){
  
  # split data into training and test set
  nrOfTrainingSamples <- round(percentage*nrow(data), 0)
  training.indeces <- sample(1:nrow(data), nrOfTrainingSamples, replace = F)
                      
  training.data.set <- data[training.indeces,]
  training.classification.set <- classification[training.indeces]
  
  validation.data.set <- data[-training.indeces,]
  validation.classification.set <- classification[-training.indeces]
  
  # train SVM with training set
  classifier <- svm(x = data, 
                    y = classification, 
                    type = "C-classification",
                    kernel = kernel, 
                    cost = 10, 
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