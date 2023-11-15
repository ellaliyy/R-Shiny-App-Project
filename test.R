library(dplyr)
library(tidyr)
source("util.R")

#query_kwds
# Example usage: Search for studies with "cancer" in their title.
query_studied <- query_kwds(studies, kwds = "cancer", column = "brief_title") |> collect()
print(query_studied |> head(10))

#plot_phase_histogram
histogram <- plot_phase_histogram(query_studied)
print(histogram)

#plot the concurrent studies
plot_concurrent_studies(query_studied)


