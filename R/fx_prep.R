# Experis Business Demo ---------------------------------------------------

# Function: Prepare

demo_prepare <- function(data = NULL,
                         data_path = NULL,
                         data_delim = ",",
                         strings_to_factor = TRUE,
                         col_id = NULL,
                         cols_new = NULL,
                         cols_drop = NULL,
                         row_filter = NULL,
                         p_training = 0.60,
                         sample_group = NULL,
                         create_segments = FALSE,
                         segment_k = 20,
                         segment_estimate_k = TRUE) {

  library(tidyverse)
  library(modelr)
  library(rlang)
  source("R/fx_segment.R")


  if (data %>% is_empty()) {

    if (data_path %>% file.exists() %>% is_false()) abort("File does not exist!")
    data <- read_delim(data_path, delim = data_delim)

  }

  # Factors
  if (strings_to_factor == TRUE) {
    data <- data %>% mutate_if(is_character, as_factor)
  }


  # Add Rownames
  if (col_id %>% is_empty()) {
    col_id <- "rowid"
    data <- data %>% rowid_to_column(col_id)
  }

  # Vector of Columns
  data_cols_keep <-
    c(colnames(data), names(cols_new)) %>%
    setdiff(cols_drop) %>%
    unique()


  # Prepare rlang
  rlang_mutate <- if (cols_new %>% is_empty()) NULL else cols_new %>% map(parse_expr)
  rlang_filter <- if (row_filter %>% is_empty()) NULL else row_filter %>% parse_expr()
  rlang_select <- syms(data_cols_keep)

  rlang_stratify_group <- if (sample_group %>% is_empty()) NULL else syms(sample_group)
  rlang_filter_rowid   <- str_glue("!{col_id} %in% data_train[['{col_id}']]") %>% parse_expr()

  # Manipulate Data
  data_prep <-
    data %>%
    mutate(!!! rlang_mutate) %>%
    filter(!!! rlang_filter)

  # Segment
  segment_model <- NULL

  if (create_segments %>% is_true()) {

    inform("Segmenting Data")

    segment_model <-
      demo_segment(data = data_prep,
                   k = segment_k,
                   estimate_k = segment_estimate_k,
                   cols_remove = c(col_id, cols_drop))

    data_prep <-
      data_prep %>%
      add_column(segment = factor(segment_model$assignment))

    data_cols_keep <- union(data_cols_keep, "segment") %>% unique()
    rlang_select <- syms(data_cols_keep)

  }

  # Split
  data_split_index <-
    data_prep %>%
    group_by(!!! rlang_stratify_group) %>%
    resample_partition(p = c(train = p_training,
                             holdout = 1 - p_training))

  data_split <-
    data_split_index %>%
    map(~ .x %>%
          as_tibble() %>%
          ungroup() %>%
          select(!!! rlang_select))



  list(train = data_split$train,
       holdout = data_split$holdout,
       segment = segment_model$summary
  )


}



# Test Function -----------------------------------------------------------

# demo_prepare(data = NULL,
#              data_path = "data/bank.csv",
#              data_delim = ";",
#              col_id = NULL,
#              cols_new = list(response = "ifelse(y == 'yes', 1, 0)"),
#              cols_drop = "y",
#              row_filter = NULL,
#              p_training = 0.60,
#              sample_group = "y",
#              create_segments = TRUE,
#              segment_k = 20,
#              segment_estimate_k = TRUE,
#              strings_to_factor = TRUE)

data = NULL
data_path = "data/bank.csv"
data_delim = ";"
col_id = NULL
cols_new = list(response = "ifelse(y == 'yes', 1, 0)")
cols_drop = "y"
row_filter = NULL
p_training = 0.60
sample_group = "y"
create_segments = TRUE
segment_k = 20
segment_estimate_k = FALSE
strings_to_factor = TRUE
