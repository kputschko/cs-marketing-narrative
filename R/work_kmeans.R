
# Segmentation ------------------------------------------------------------

pacman::p_load(tidyverse, h2o)
h2o.init()

data <- read_delim("data/bank.csv", delim = ";")
data_h2o <- data %>% select(-y) %>% as.h2o()
data_y <- data %>% select(y) %>% as.h2o()

clusters <-
  h2o.kmeans(data_h2o,
             nfolds = 5,
             keep_cross_validation_fold_assignment = TRUE,
             # keep_cross_validation_predictions = TRUE,
             estimate_k = TRUE,
             seed = 43,
             max_iterations = 20,
             k = 20)

clusters@model$model_summary
clusters@model$scoring_history
clusters@model$cross_validation_metrics
clusters@model$cross_validation_metrics_summary
clusters@model$help
clusters@model$cross_validation_models
clusters@model$cross_validation_holdout_predictions_frame_id
clusters@model$cross_validation_fold_assignment_frame_id

data_with_clusters <-
  clusters@model$cross_validation_fold_assignment_frame_id$name %>% h2o.getFrame() %>%
  h2o.cbind(data_h2o, data_y)

h2o.centers(clusters)
h2o.cluster_sizes(clusters)
h2o.centersSTD(clusters)
h2o.centroid_stats(clusters, xval = TRUE)
h2o.num_iterations(clusters)

clusters@model$centers

clusters@model$scoring_history %>%
  ggplot() +
  aes(x = iterations, y = within_cluster_sum_of_squares) +
  aes(x = iterations, y = within_cluster_sum_of_squares) +
  geom_line()

# A Function --------------------------------------------------------------

demo_segment <- function(data_h2o, search_k = 20) {

  library(h2o)

  summary <- h2o.describe(data_h2o)

  if ("string" %in% summary$type) {
    warn("String columns detected in data.  Consider converting to factor prior to clustering")
  }

  model_cluster <-
    h2o.kmeans(data_h2o,
               seed = 43,
               nfolds = 5,
               k = search_k,
               estimate_k = TRUE,
               keep_cross_validation_fold_assignment = TRUE)

  cluster_summary <- model_cluster %>% h2o.centers()

  cluster_assignment <-
    model_cluster@model$cross_validation_fold_assignment_frame_id$name %>%
    h2o.getFrame()


  list(model = model_cluster,
       summary = cluster_summary,
       assignment = cluster_assignment)


}

data_h2o %>% demo_segment()
