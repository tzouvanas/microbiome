# plot timepoints
plots.timepoints <- function(x, y, samples, timepoints){
  fig_by_month <- plot_ly(x = x, y = y,
          color = timepoints, colors = brewer.pal(n = length(timepoints), name = "Set1"),
          type = "scatter", mode = "markers") %>% 
          layout(title = 'Timepoint')
  fig_by_month
}

# plot samples
plots.samples <- function(x, y, samples, timepoints){
	fig_by_sample <- plot_ly(x = x, y = y,
							 color = samples, colors = brewer.pal(n = length(samples), name = "Set1"),
							 text = timepoints,
							 type = "scatter", mode = "markers") %>% 
			layout(title = 'Samples') %>% 
			add_text(textposition = "top right")
	fig_by_sample
}

# plot classification
plots.classification <- function(x, y, centroid_allocations){
  fig_by_classification <- plot_ly(x = x, y = y,
           color = as.vector(centroid_allocations), colors = brewer.pal(n = length(centroid_allocations), name = "Set1"),
           type = "scatter", mode = "markers")
   fig_by_classification <- fig_by_classification %>% layout(title = 'Classification')
   fig_by_classification
}

#plot specific individual
plots.individual <- function(x, y, samples, timepoints, individual){
  
  individual.index <- which(startsWith(samples, paste("X", individual, sep = '')))
  
  x.individual <- x[individual.index]
  y.individual <- y[individual.index]
  
  individual.timepoints <- timepoints[individual.index]
  individual.samples <- samples[individual.index]
  
  fig_by_sample <- plot_ly(x = x.individual, y = y.individual,
                           text = individual.timepoints,
                           type = "scatter", mode = "markers") %>% 
    layout(title = individual) %>% 
    layout(showlegend = FALSE) %>% 
    add_text(textposition = "top right")
  fig_by_sample
}
  
  
  
  
  
  