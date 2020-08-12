# plot scaled distances by month
otuplots.plot.timepoints <- function(x, y, samples, timepoints){
  fig_by_month <- plot_ly(x = x, y = y,
          color = timepoints, colors = brewer.pal(n = length(timepoints), name = "Set1"),
          type = "scatter", mode = "markers") %>% 
          layout(title = 'Sample distances by timepoint')
  fig_by_month
}

# plot distances by sample
otuplots.plot.samples <- function(x, y, samples, timepoints){
	fig_by_sample <- plot_ly(x = x, y = y,
							 color = samples, colors = brewer.pal(n = length(samples), name = "Set1"),
							 text = timepoints,
							 type = "scatter", mode = "markers") %>% 
			layout(title = 'Sample distances') %>% 
			add_text(textposition = "top right")
	fig_by_sample
}

# plot distances by classification
otuplots.plot.classification <- function(x, y, centroid_allocations){
  fig_by_classification <- plot_ly(x = x, y = y,
           color = as.vector(centroid_allocations), colors = brewer.pal(n = length(centroid_allocations), name = "Set1"),
           type = "scatter", mode = "markers")
   fig_by_classification <- fig_by_classification %>% layout(title = 'Sample distances by classification')
   fig_by_classification
}