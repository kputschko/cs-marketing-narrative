
# Experis Demo ------------------------------------------------------------

pacman::p_load(tidyverse, foreach, rlang, h2o, scales)

# Source:
# [Moro et al., 2014] S. Moro, P. Cortez and P. Rita. A Data-Driven Approach to Predict the Success of Bank Telemarketing. Decision Support Systems, Elsevier, 62:22-31, June 2014

# Prevous research here:
# https://rstudio-pubs-static.s3.amazonaws.com/157695_7fe1c26be1b54b01a520a9b73ae99c85.html


# GOALS -------------------------------------------------------------------
# Shiny
# Three tabs: prepare, model, score
# prepare - ggplot qplot to view selected column; remove cols; create cols; filter rows; select csv source
# model - select rds data source; select models to build; view plots; view summary table; select model source to explore
# score - select model rds source; explore model source; select rds data to score; view summary table; view lift table

# Model get model parameters, ntrees etc.



d_bank <-
  read_delim("data/bank.csv",  delim = ";",
             col_types = cols(age = col_integer(),
                              job = col_character(),
                              marital = col_character(),
                              education = col_character(),
                              default = col_character(),
                              housing = col_character(),
                              loan = col_character(),
                              contact = col_character(),
                              month = col_character(),
                              day_of_week = col_character(),
                              duration = col_integer(),
                              campaign = col_integer(),
                              pdays = col_integer(),
                              previous = col_integer(),
                              poutcome = col_character(),
                              emp.var.rate = col_double(),
                              cons.price.idx = col_double(),
                              cons.conf.idx = col_double(),
                              euribor3m = col_double(),
                              nr.employed = col_double(),
                              y = col_character()
             )) %>%
  mutate(pdays = ifelse(pdays == 999, NA, pdays),
         y = ifelse(y == "yes", 1, 0))



# Prepare Raw Data --------------------------------------------------------

demo_prepare <- function(data = NULL,
                         data_path = NULL,
                         data_delim = ",",
                         sample_group = NULL,
                         p_training = 0.60,
                         cols_new = NULL,
                         cols_drop = NULL,
                         col_id = NULL,
                         row_filter = NULL) {

  library(tidyverse)
  library(rlang)

  if (data %>% is_empty()) {

    if (data_path %>% file.exists() %>% is_false()) abort("File does not exist!")
    data <- read_delim(data_path, delim = data_delim)

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
    filter(!!! rlang_filter) %>%
    group_by(!!! rlang_stratify_group)

  data_train <-
    data_prep %>%
    sample_frac(p_training) %>%
    ungroup() %>%
    select(!!! rlang_select)

  data_new <-
    data_prep %>%
    filter(!!! rlang_filter_rowid) %>%
    ungroup() %>%
    select(!!! rlang_select)


  list(train = data_train,
       new = data_new)


}


run_prep <-
  demo_prepare(
    data_path = "data/bank.csv",
    data_delim = ";",
    cols_new = list(pdays = "ifelse(pdays == 999, NA, pdays)",
                    response = "ifelse(y == 'yes', 1, 0)"),
    cols_drop = "y",
    col_id = NULL,
    row_filter = NULL,
    sample_group = "response",
    p_training = 0.60
  )


# Export Data
write_rds(run_prep$train, "data/prepared_training.rds", compress = "gz")
write_rds(run_prep$new, "data/prepared_new_data.rds", compress = "gz")



# Build Models ------------------------------------------------------------

demo_model <- function(data = NULL,
                       data_path = NULL,
                       response,
                       col_id = NULL,
                       algorithms = c("drf", "gbm", "lr", "dl")) {

  library(tidyverse)
  library(scales)
  library(rlang)
  library(h2o)
  source("R/fx_utility_functions.r")


  # | Import Training Data ----
  inform("Importing Data")

  if (data %>% is_empty()) {

    if (data_path %>% file.exists() %>% is_false()) abort("File does not exist!")
    data <- read_rds(data_path)

  }

  # | Variable Summary ----
  inform("Getting Summaries")
  metadata_variable_summary  <- data %>% .fx_describe()
  metadata_response_summary  <- metadata_variable_summary %>% filter(column_name %in% response)
  metadata_character_summary <- metadata_variable_summary %>% filter(column_type == "character")


  # | Model Variables ----
  metadata_model_vars <-
    metadata_variable_summary %>%
    pull(column_name) %>%
    setdiff(response) %>%
    setdiff(col_id)

  stopifnot(response %in% metadata_variable_summary$column_name)


  # | H2O ----
  inform("Launch H2O")
  h2o.init()

  inform("Import H2O Data")
  h2o_training <- data %>% as.h2o(destination_frame = "training")


  # | H2O Factors ----
  metadata_factor_columns <-
    c(metadata_character_summary$column_name,
      metadata_response_summary$column_name) %>%
    sort() %>%
    unique()


  metadata_nonfactor_columns <-
    metadata_variable_summary %>%
    pull(column_name) %>%
    setdiff(metadata_factor_columns)


  # H2O Factor Loop
  h2o_factors_individual <-
    foreach(i = seq_along(metadata_factor_columns), .errorhandling = "pass") %do% {

      .loop_col <- metadata_factor_columns[i]
      .loop_key <- str_c("factor", .loop_col, sep = "_")


      # H2O
      .loop_factor <- h2o.asfactor(h2o_training[[.loop_col]])

      h2o.assign(.loop_factor, .loop_key)
      h2o.rm(.loop_factor)


      h2o.getFrame(.loop_key)

    }


  # Error Check: Factor Loop
  h2o_factors_keep <-
    h2o_factors_individual %>%
    keep(~ class(.) == "H2OFrame")

  if (is_empty(h2o_factors_keep)) {abort("Factors did not convert sucessfully!")}


  # Finalize Factors
  h2o_factors_combined <- h2o.cbind(h2o_factors_keep, h2o_training[ , metadata_nonfactor_columns])


  # H2O
  h2o_training_factors <- h2o.assign(h2o_factors_combined, key = "training_factors")
  h2o.rm(h2o_factors_combined)


  # | Model ----
  metadata_models <-
    foreach(alg = algorithms, .combine = "rbind") %do% {
      runtime <- system.time({

        model_id <- str_glue("{Sys.Date()}_{alg}")

        model <-
          if (alg == "gbm") {

            inform("Building the GBM")

            h2o.gbm(
              seed = 42,
              nfolds = 5,
              y = response,
              model_id = model_id,
              balance_classes = TRUE,
              x = metadata_model_vars,
              training_frame = h2o_training_factors)

          } else if (alg == "drf") {

            inform("Building the DRF")

            h2o.randomForest(
              seed = 42,
              nfolds = 5,
              y = response,
              model_id = model_id,
              balance_classes = TRUE,
              x = metadata_model_vars,
              training_frame = h2o_training_factors)

          } else if (alg == "lr") {

            inform("Building the LR")

            h2o.glm(
              seed = 42,
              nfolds = 5,
              y = response,
              family = "binomial",
              model_id = model_id,
              balance_classes = TRUE,
              x = metadata_model_vars,
              remove_collinear_columns = TRUE,
              training_frame = h2o_training_factors
            )

          } else if (alg == "dl") {

            inform("Building the DL")

            h2o.deeplearning(
              seed = 42,
              nfolds = 2,
              y = response,
              hidden = c(50, 50),
              model_id = model_id,
              stopping_rounds = 2,
              balance_classes = TRUE,
              x = metadata_model_vars,
              training_frame = h2o_training_factors
            )

          }

      })[[3]]


      tibble(algorithm = alg,
             runtime,
             response,
             model = lst(model)) %>%
        filter(runtime > 0)


    }


  # | Results ----
  inform("Gathering Results")

  metadata_results <-
    metadata_models %>%
    mutate(model_id = map_chr(model, ~ .x %>% pluck("model_id")),
           auc      = map_dbl(model, h2o.auc, xval = TRUE),
           rmse     = map_dbl(model, h2o.rmse, xval = TRUE),
           lift     = map(model, h2o.gainsLift, xval = TRUE),
           varimp   = map(model, h2o.varimp),
           summary  = map(model, ~.x %>%
                            pluck("model") %>%
                            pluck("model_summary")),
           roc      = map(model, ~ .x %>%
                            pluck("model") %>%
                            pluck("cross_validation_metrics") %>%
                            pluck("metrics") %>%
                            pluck("thresholds_and_metric_scores")),
           champion = if_else(auc == max(auc), 1, 0))


  # | Export Models ----
  metadata_results %>%
    filter(champion == 1) %>%
    pull(model) %>%
    walk(~ .x %>% h2o.saveModel("data/model"))


  # | Plot Results ----
  inform("Creating Plots")

  .plot_theme <-
    theme_minimal() +
    theme(strip.text.y = element_text(angle = 0, face = "bold"),
          strip.text.x = element_text(face = "bold"),
          panel.background = element_rect(color = "gray"),
          panel.grid.major.x = element_line(color = "gray", linetype = 3),
          panel.grid.minor.x = element_line(color = "gray", linetype = 3),
          legend.title.align = 0.5)


  plot_capture <-
    metadata_results %>%
    select(algorithm, lift) %>%
    deframe() %>%
    map(as_tibble) %>%
    bind_rows(.id = "algorithm") %>%
    ggplot() +
    aes(x = cumulative_data_fraction, y = cumulative_capture_rate, color = str_to_upper(algorithm)) +
    geom_line(linetype = "dashed") +
    geom_abline(slope = 1, intercept = 0, color = "black") +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Population Percent",
         y = NULL,
         title = "Cumulative Capture Rate",
         color = "Model") +
    .plot_theme


  plot_lift <-
    metadata_results %>%
    select(algorithm, lift) %>%
    deframe() %>%
    map(as_tibble) %>%
    bind_rows(.id = "algorithm") %>%
    ggplot() +
    aes(x = cumulative_data_fraction, y = cumulative_lift, color = str_to_upper(algorithm)) +
    geom_line() +
    geom_hline(aes(yintercept = 1), color = "black") +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Population Percent",
         y = NULL,
         color = "Model",
         title = "Cumulative Lift") +
    .plot_theme


  plot_roc <-
    metadata_results %>%
    select(algorithm, roc) %>%
    deframe() %>%
    map(as_tibble) %>%
    bind_rows(.id = "algorithm") %>%
    ggplot() +
    aes(x = fpr, y = tpr, color = str_to_upper(algorithm)) +
    geom_line(linetype = "dashed") +
    geom_abline(slope = 1, intercept = 0, color = "black") +
    labs(color = "Model",
         x = "False Positive Rate",
         y = "True Positive Rate",
         title = "ROC Curve") +
    .plot_theme


  .n_distinct_algs <- metadata_results %>% n_distinct("algorithm")

  plot_varimp <-
    metadata_results %>%
    select(algorithm, varimp) %>%
    deframe() %>%
    map(as_tibble) %>%
    map_if(names(.) == "lr",
           ~ .x %>% transmute(variable = names, scaled_importance = rescale(coefficients))) %>%
    bind_rows(.id = "algorithm") %>%
    group_by(algorithm) %>%
    top_n(10, scaled_importance) %>%
    group_by(variable) %>%
    summarise(sum_score = sum(scaled_importance) / !!.n_distinct_algs,
              n_score = length(scaled_importance)) %>%
    arrange(sum_score) %>%
    mutate(variable = as_factor(variable)) %>%
    ggplot() +
    aes(x = variable, y = sum_score, fill = n_score) +
    geom_col(color = "black") +
    scale_fill_viridis_c(option = "E", alpha = 0.75) +
    coord_flip() +
    labs(x = NULL,
         y = "Scaled Importance",
         fill = "Number of Models",
         title = "Variable Importance") +
    .plot_theme


  metadata_plots <-
    ls(pattern = "plot_") %>%
    mget(inherits = TRUE) %>%
    enframe()


  # | Shutdown H2O ----
  h2o.shutdown(prompt = FALSE)




  # | List Output for Debugging ----
  lst(metadata_variable_summary,
      metadata_response_summary,
      metadata_character_summary,
      metadata_model_vars,
      metadata_results,
      metadata_plots)

}


run_model <-
  demo_model(
    data_path = "data/train/2018-09-17_train_data.rds",
    response = "response",
    col_id = "rowid",
    algorithms = c("lr", "drf", "gbm", "dl")
  )


run_model %>% write_rds("data/model/model_metadata.rds", compress = "gz")


# Score New Data ----------------------------------------------------------

demo_score <-
  function(data = NULL,
           model = NULL,
           path_data = NULL,
           path_model = NULL,
           col_id = NULL) {

    library(tidyverse)
    library(rlang)
    library(h2o)

    if (data %>% is_empty()) {data <- read_rds(data_path)}

    if (model %>% is_empty()) {metadata_models <- read_rds(path_model)}

    # H2O
    inform("Start H2O")
    h2o.init()
    data_h2o <- data %>% as.h2o()

    # | H2O Model ----
    inform("Load H2O Model")
    metadata_champion <-
      metadata_models %>%
      pluck("metadata_results") %>%
      filter(champion == 1) %>%
      pull(model_id) %>%
      str_c("data/model/", .) %>%
      h2o.loadModel()


    # | Score ----
    inform("Score in H2O")
    score <-
      metadata_champion %>%
      h2o.predict(data_h2o) %>%
      h2o.cbind(data_h2o[[col_id]]) %>%
      as_tibble()


    # | Lift Targeting Table ----
    inform("Score Summary Table")
    lift_table <-
      score %>%
      mutate(group = ntile(-p1, 20)) %>%
      group_by(group) %>%
      summarise_at(vars(p1), funs(lower_threshold = "min",
                                  response_rate = "mean",
                                  "sd",
                                  n = "length")) %>%
      arrange(desc(response_rate)) %>%
      ungroup() %>%
      mutate(lift = response_rate / mean(response_rate),
             cumulative_n = cumsum(n),
             cumulative_data_fraction = cumulative_n / sum(n),
             cumulative_lift = cummean(lift),
             cumulative_response_rate = cummean(response_rate)) %>%
      select(group, lower_threshold,
             n, cumulative_n, cumulative_data_fraction,
             response_rate, cumulative_response_rate,
             lift, cumulative_lift)


    # | Plot ----
    .plot_theme <-
      theme_minimal() +
      theme(strip.text.y = element_text(angle = 0, face = "bold"),
            strip.text.x = element_text(face = "bold"),
            panel.background = element_rect(color = "gray"),
            panel.grid.major.x = element_line(color = "gray", linetype = 3),
            panel.grid.minor.x = element_line(color = "gray", linetype = 3),
            legend.title.align = 0.5)

    .target_point <- lift_table %>% filter(lift > 1) %>% filter(lift == min(lift)) %>% pull(cumulative_data_fraction)

    lift_plot <-
      lift_table %>%
      ggplot() +
      aes(x = cumulative_data_fraction, y = cumulative_lift) +
      geom_line(size = 1, color = "red") +
      geom_hline(aes(yintercept = 1)) +
      geom_vline(aes(xintercept = .target_point), linetype = "dashed", color = "gray") +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = "Population Percent",
           y = NULL,
           color = "Model",
           title = "Cumulative Lift") +
      .plot_theme


    # | Shutdown ----
    # h2o.shutdown(prompt = FALSE)


    # | Results ----
    lst(lift_table,
        lift_plot,
        score)

  }


run_score <-
  demo_score(path_data = "data/train/2018-09-17_holdout_data.rds",
             path_model = "data/model/2018-09-18_model.rds",
             col_id = "rowid")


run_score %>%
  enframe() %>%
  write_rds("data/score/metadata_score.rds", compress = "gz")
