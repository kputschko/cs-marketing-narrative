# Experis Business Demo ---------------------------------------------------

# Function: Segment

demo_segment <- function(data, k = 20, estimate_k = TRUE, cols_include = NULL, cols_remove = NULL) {

  library(h2o)
  h2o.init()

  inform("Convert to H2O")

  data_h2o <-
    if ("H2OFrame" %in% class(data)) {
      data
    } else {
      data %>% mutate_if(is_character, as_factor) %>% as.h2o()
    }


  # Remove Columns
  cols_all <- h2o.colnames(data_h2o)
  x <-
    if (cols_include %>% is_empty()) {
      setdiff(cols_all, cols_remove)
    } else {
        cols_include
    }


  # Create Segments
  inform("Build Segments")

  segment_model <-
    h2o.kmeans(data_h2o,
               k = k,
               seed = 42,
               # nfolds = 5,
               x = x,
               estimate_k = estimate_k)


  inform("Segment Assignments")

  segment_summary <- segment_model %>% h2o.centers() %>% as_tibble() %>% rownames_to_column("segment")
  segment_assignment <- h2o.predict(segment_model, data_h2o) %>% as.vector() %>% magrittr::add(1)


  inform("Segmentation Done")

  lst(
    model = segment_model,
    summary = segment_summary,
    assignment = segment_assignment
    )



}


# Test --------------------------------------------------------------------

# data <- "data/bank.csv" %>% read_delim(delim = ";")
#
# k <- 20
# estimate_k <- TRUE
# # cols_remove <- "mpg"
# # cols_include <- c("mpg", "cyl")
# cols_remove <- NULL
# cols_include <- NULL
#
#
# test_segment <-
#   demo_segment(
#     data = data,
#     k = k,
#     estimate_k = estimate_k,
#     cols_include = cols_include,
#     cols_remove = cols_remove)
#
# test_segment$model@model$training_metrics

