# Experis Business Demo ---------------------------------------------------

# Function: Model

demo_model <- function(data = NULL,
                       data_path = NULL,
                       response,
                       group = NULL,
                       col_id = NULL,
                       automl_runtime = 120,
                       algorithms = c("drf", "gbm", "lr", "dl")) {



  library(tidyverse)
  library(scales)
  library(rlang)
  library(h2o)
  source("R/fx_utility_functions.r")
  source("R/fx_h2o_asfactor.r")
  source("R/fx_segment.r")


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

  h2o_training_factors <-
    fx_h2o_asfactor(h2o_training,
                    cols_to_factor = metadata_factor_columns,
                    key = "training_factors")


  # | H2O Nest ----
  if (group %>% is_empty()) {

    inform("We are not nesting the data")
    h2o_training_factors <- h2o.assign(h2o_training_factors, key = "training_data")
    group_nest_final <- tibble(group_id = "training_data")

  } else {

    group_distinct <-
      data %>%
      distinct(!!! syms(group)) %>%
      rowid_to_column()

    group_label <-
      group_distinct %>%
      gather(column, value, -rowid) %>%
      unite(colval, column, value, remove = FALSE) %>%
      arrange(rowid) %>%
      group_by(rowid) %>%
      summarise(group_id = colval %>% str_c(collapse = "__"),
                data_filter = str_glue("{column} == '{value}'") %>% str_c(collapse = " & ")) %>%
      left_join(group_distinct, by = "rowid")

    group_index <-
      group_label %>%
      select(!!! syms(group)) %>%
      gather(column, value) %>%
      distinct() %>%
      mutate(h2o_key_index = str_c("index", column, value, sep = "_"))

    pwalk(list(group_index$h2o_key_index,
               group_index$column,
               group_index$value),
          ~ h2o.assign(key = ..1, h2o_training_factors[[..2]] == ..3))

    group_nest <-
      group_label %>%
      select(rowid, !!! syms(group)) %>%
      gather(column, value, -rowid) %>%
      left_join(group_index, by = c("column", "value")) %>%
      arrange(rowid) %>%
      left_join(group_label, by = "rowid")

    group_loop <-
      group_nest %>%
      distinct(group_id) %>%
      pull()

    group_list <-
      foreach(i = seq_along(group_loop)) %do% {

        loop_data_id  <- group_loop[i]
        loop_data     <- group_nest %>% filter(group_id == loop_data_id)
        loop_group_id <- loop_data %>% distinct(rowid) %>% pull()

        index_combine <-
          loop_data %>%
          pull(h2o_key_index) %>%
          map(h2o.getFrame) %>%
          reduce("*") %>%
          h2o.which() %>%
          as.vector()

        inform(str_glue("{i}: Nesting {loop_data_id}"))

        lst(!!sym(loop_data_id) := index_combine)

      }

    group_nest_final <-
      group_list %>%
      flatten() %>%
      enframe("group_id", "row_index")


    pwalk(list(group_nest_final$group_id,
               group_nest_final$row_index),
          ~ h2o.assign(key = ..1, data = h2o_training_factors[..2, ]))

    gc()

  }


  # | Model ----
  metadata_models <-
    foreach(i = seq_len(group_nest_final %>% nrow())) %:%
    foreach(loop_model_algorithm = algorithms, .combine = "rbind") %do% {

      runtime <- system.time({

        loop_model_group <- group_nest_final$group_id[[i]]

        model_id <- str_glue("{Sys.Date()}--{loop_model_algorithm}--{response}--{loop_model_group}")

        inform(str_glue("Building the {model_id}"))

        training_frame <- h2o.getFrame(loop_model_group)



        model <-
          if (loop_model_algorithm == "gbm") {

            h2o.gbm(
              seed = 42,
              nfolds = 5,
              y = response,
              model_id = model_id,
              balance_classes = TRUE,
              x = metadata_model_vars,
              training_frame = training_frame)

          } else if (loop_model_algorithm == "drf") {

            h2o.randomForest(
              seed = 42,
              nfolds = 5,
              y = response,
              model_id = model_id,
              balance_classes = TRUE,
              x = metadata_model_vars,
              training_frame = training_frame)

          } else if (loop_model_algorithm == "lr") {

            h2o.glm(
              seed = 42,
              nfolds = 5,
              y = response,
              family = "binomial",
              model_id = model_id,
              balance_classes = TRUE,
              x = metadata_model_vars,
              remove_collinear_columns = TRUE,
              training_frame = training_frame
            )

          } else if (loop_model_algorithm == "dl") {

            h2o.deeplearning(
              seed = 42,
              nfolds = 2,
              y = response,
              hidden = c(50, 50),
              model_id = model_id,
              stopping_rounds = 2,
              balance_classes = TRUE,
              x = metadata_model_vars,
              training_frame = training_frame
            )

          } else if (loop_model_algorithm == "auto") {

            h2o.automl(
              seed = 42,
              nfolds = 5,
              y = response,
              balance_classes = TRUE,
              max_runtime_secs = automl_runtime,
              max_models = 10,
              x = metadata_model_vars,
              training_frame = training_frame
            )

          }

      })[[3]]


      tibble(algorithm = loop_model_algorithm,
             group_id = loop_model_group,
             runtime,
             response,
             model = lst(model)) %>%
        filter(runtime > 0)


    }


  # | Results ----
  inform("Gathering Results")

  metadata_results <-
    metadata_models %>%
    bind_rows() %>%
    mutate(model_id = map_chr(model, ~ .x %>% pluck("model_id")),
           auc      = map_dbl(model, h2o.auc, xval = TRUE),
           rmse     = map_dbl(model, h2o.rmse, xval = TRUE),
           lift     = map(model, h2o.gainsLift, xval = TRUE),
           varimp   = map(model, h2o.varimp),
           summary  = map(model, ~.x %>%
                            pluck("model") %>%
                            pluck("model_summary") %>%
                            as_tibble()),
           roc      = map(model, ~ .x %>%
                            pluck("model") %>%
                            pluck("cross_validation_metrics") %>%
                            pluck("metrics") %>%
                            pluck("thresholds_and_metric_scores"))) %>%
    group_by(group_id, response) %>%
    mutate(champion = if_else(auc == max(auc), 1, 0)) %>%
    ungroup()


  # | Export Models ----
  # metadata_results %>%
  #   filter(champion == 1) %>%
  #   pull(model) %>%
  #   walk(~ .x %>% h2o.saveModel("data/model", force = TRUE))


  # | Plot Results ----
  inform("Creating Plots")

  data_lift_capture <-
    metadata_results %>%
    select(algorithm, response, group_id, lift, champion) %>%
    unite(label, response, group_id) %>%
    unnest(lift)


  plot_capture <-
  data_lift_capture %>%
    ggplot() +
    aes(x = cumulative_data_fraction,
        y = cumulative_capture_rate,
        color = str_to_upper(algorithm)) +
    geom_line(linetype = "dashed") +
    geom_line(data = data_lift_capture %>% filter(champion == 1),
              linetype = "solid") +
    geom_abline(slope = 1, intercept = 0, color = "black") +
    scale_y_continuous(limits = c(0, NA)) +
    facet_wrap(vars(label)) +
    labs(x = "Population Percent",
         y = NULL,
         title = "Cumulative Capture Rate",
         color = "Model") +
    .plot_theme


  plot_lift <-
    data_lift_capture %>%
    ggplot() +
    aes(x = cumulative_data_fraction,
        y = cumulative_lift,
        color = str_to_upper(algorithm)) +
    geom_line(linetype = "dashed") +
    geom_line(data = data_lift_capture %>% filter(champion == 1),
              linetype = "solid") +
    geom_hline(aes(yintercept = 1), color = "black") +
    scale_y_continuous(limits = c(0, NA)) +
    facet_wrap(vars(label), scales = "free_y") +
    labs(x = "Population Percent",
         y = NULL,
         color = "Model",
         title = "Cumulative Lift") +
    .plot_theme


  plot_roc <-
    metadata_results %>%
    select(algorithm, response, group_id, roc, champion) %>%
    unite(label, response, group_id) %>%
    unnest(roc) %>%
    ggplot() +
    aes(x = fpr, y = tpr, color = str_to_upper(algorithm)) +
    geom_line(linetype = "dashed") +
    geom_line(data = . %>% filter(champion == 1),
              linetype = "solid") +
    geom_abline(slope = 1, intercept = 0, color = "black") +
    facet_wrap(vars(label)) +
    labs(color = "Model",
         x = "False Positive Rate",
         y = "True Positive Rate",
         title = "ROC Curve") +
    .plot_theme


  .n_distinct_algs <- metadata_results$algorithm %>% n_distinct()


  plot_varimp <-
    metadata_results %>%
    select(algorithm, response, group_id, varimp, champion) %>%
    unite(label, response, group_id) %>%
    mutate(varimp =
             if_else(condition = algorithm == "lr",
                     true = map(varimp,
                                ~ .x %>%
                                  as_tibble() %>%
                                  rename(column = 1, value = 2) %>%
                                  mutate(method = "coefficient",
                                         value = rescale(value))),
                     false = map(varimp,
                                 ~ .x %>%
                                   as_tibble() %>%
                                   select(column = 1, value = 3) %>%
                                   mutate(method = "scaled_importance",
                                          sign = "POS"))
    )) %>%
    unnest(varimp) %>%
    group_by(label, algorithm) %>%
    top_n(5, value) %>%
    ggplot() +
    aes(x = column, y = value, group = method) +
    geom_col(color = "black") +
    coord_flip() +
    facet_grid(cols = vars(method, algorithm),
               rows = vars(label),
               scales = "free",
               space = "free") +
    labs(x = NULL, y = NULL, title = "Variable Importance") +
    .plot_theme

  # plot_varimp <-
  #   metadata_results %>%
  #   select(algorithm, response, group_id, varimp, champion) %>%
  #   unite(label, response, group_id) %>%
  #   unnest(varimp) %>%
  #   mutate(variable = ifelse(algorithm == "lr", names, variable),
  #          scaled_importance = ifelse(algorithm == "lr", rescale(coefficients), scaled_importance)) %>%
  #   select(-coefficients, -sign, -relative_importance, -percentage, -names) %>%
  #   group_by(label, algorithm) %>%
  #   top_n(5, scaled_importance) %>%
  #   ggplot() +
  #   aes(x = fct_infreq(variable) %>% fct_rev(),
  #       y = scaled_importance,
  #       fill = str_to_upper(algorithm)) +
  #   geom_col() +
  #   facet_grid(label ~ str_to_upper(algorithm), scales = "free") +
  #   labs(x = NULL, y = NULL, title = "Variable Importance", fill = NULL) +
  #   guides(fill = "none") +
  #   coord_flip() +
  #   .plot_theme


  metadata_plots <-
    ls(pattern = "plot_") %>%
    mget(inherits = TRUE) %>%
    enframe()


  # | Shutdown H2O ----
  # inform("Shutdown H2O")
  # h2o.shutdown(prompt = FALSE)




  # | List Output for Debugging ----
  inform("Store Output")
  lst(metadata_variable_summary,
      metadata_response_summary,
      metadata_character_summary,
      metadata_model_vars,
      metadata_results,
      metadata_plots)

}



# Test Run ----------------------------------------------------------------

# data <- NULL
# data_path <- "data/train/2018-09-26_train_data.rds"
# response <- "response"
# group <- NULL
# col_id <- "rowid"
# algorithms <- "lr"
# algorithms <- "gbm"
# algorithms <- c("lr", "gbm")
# algorithms <- c("lr", "drf", "gbm", "dl")


# run_model <-
#   demo_model(
#     data = data,
#     data_path = data_path,
#     response = response,
#     group = group,
#     col_id = col_id,
#     algorithms = algorithms)

