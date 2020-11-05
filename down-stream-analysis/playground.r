
timepoint.list <- c("01", "03", "05", "07", "09", "12", "24", "MM")
#timepoint.list <- c("12")
samples.of.timepoints <- samples[substr(samples, start = 5, stop = 7) %in% timepoint.list]
otus.of.timepoints <- otus.all.records.for.timepoints(workload$otus, timepoint.list)
otus.normalized <- otus.normalize(otus.of.timepoints, 0)
distances.of.timepoints <- distances.calculate(otus.normalized, workload$otus.tree)

# mds
mds <- cmdscale(distances.of.timepoints, eig = F, k = 2)

#plots.timepoints(mds[, 1], mds[, 2], samples.of.timepoints, timepoints[timepoints %in% timepoint.list])
#plots.samples(mds[, 1], mds[, 2], samples.of.timepoints, timepoints[timepoints %in% timepoint.list])

#plots.individual(mds[, 1], mds[, 2], samples, timepoints, "011")
#plots.individual(mds[, 1], mds[, 2], samples, timepoints, "016")

clustering <- time.series.cluster.timepoint(otus.normalized, distances.of.timepoints, timepoint.list)
plots.classification(mds[, 1], mds[, 2], centroid_allocations = clustering$best.clustering_by_ch)
plots.timepoints(mds[, 1], mds[, 2], samples, timepoints)

#SVM




