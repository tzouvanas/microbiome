environment.install.missing.packages <- function(){
  
  required.packages <- as.vector(c('ape', 'cluster', 'fpc', 'phangorn', 'GUniFrac', 'plotly', 'RColorBrewer', 'vegan'))

  already.installed.packages <- installed.packages()[,"Package"]
  
  matching.packages <- match(required.packages, already.installed.packages)
  
  missing.packages <- required.packages[which(is.na(matching.packages))]
  
  if (length(missing.packages) > 0) { install.packages(missing.packages, dependencies = T)}
}

environment.load.packages <- function(){
  
  library('ape')
  library('cluster')
  library('fpc')
  library('GUniFrac')
  library('phangorn')
  library('plotly')
  library('RColorBrewer')
  library('vegan')
}

environment.load.sources <- function(){
  
  # load .r code from local files
  source('base.r')
  source('environment.r')
  source('otu.normalization.r')
  source('otu.selectors.r')
  source('plots.r')
  source('timepoint.processing.r')
}

environment.load.dependencies <- function(){
  
  environment.install.missing.packages()
  environment.load.packages()
  environment.load.sources()
}

environment.cleanup <- function(){
  # cleanup
  cat("\014")  
  rm(list=ls(all=TRUE))
}

environment.start <- function(){
  environment.load.dependencies()
  environment.cleanup()
}