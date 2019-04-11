# Experis Business Demo ---------------------------------------------------

# Function: Score

demo_score <-
  function(data = NULL,
           model = NULL,
           path_data = NULL,
           path_model = NULL,
           col_id = NULL) {

    library(tidyverse)
    library(rlang)
    library(h2o)
    source("R/fx_utility_functions.R")

    if (data %>% is_empty()) {data <- read_rds(path_data)}

    metadata_models <- if (model %>% is_empty()) {read_rds(path_model)} else {model}

    # H2O
    inform("Start H2O")
    h2o.init()
    data_h2o <- data %>% as.h2o()


    # | H2O Model ----
    inform("Load H2O Model")
    metadata_champions <-
      metadata_models %>%
      filter(champion == 1) %>%
      select(h2o_model = model, model_id)


    # | Score ----
    inform("Score in H2O")
    score <-
      metadata_champions$h2o_model %>%
      map(~ .x %>%
            h2o.predict(data_h2o) %>%
            h2o.cbind(data_h2o[[col_id]]) %>%
            as_tibble()
      ) %>%
      set_names(metadata_champions$model_id) %>%
      bind_rows(.id = "model_id") %>%
      select(col_id, p1, model_id)



    # | Lift Targeting Table ----
    inform("Score Summary Table")
    lift_table <-
      score %>%
      group_by(model_id) %>%
      mutate(group = ntile(-p1, 20)) %>%
      group_by(model_id, group) %>%
      summarise_at(vars(p1), list(lower_threshold = "min",
                                  response_rate = "mean",
                                  "sd",
                                  n = "length")) %>%
      arrange(desc(response_rate)) %>%
      group_by(model_id) %>%
      mutate(lift = response_rate / mean(response_rate),
             cumulative_n = cumsum(n),
             cumulative_data_fraction = cumulative_n / sum(n),
             cumulative_lift = cummean(lift),
             cumulative_response_rate = cummean(response_rate)) %>%
      select(model_id, group, lower_threshold,
             n, cumulative_n, cumulative_data_fraction,
             response_rate, cumulative_response_rate,
             lift, cumulative_lift) %>%
      ungroup()


    # | Plot ----
    # .plot_theme <-
    #   theme_minimal() +
    #   theme(strip.text.y = element_text(angle = 0, face = "bold"),
    #         strip.text.x = element_text(face = "bold"),
    #         panel.background = element_rect(color = "gray"),
    #         panel.grid.major.x = element_line(color = "gray", linetype = 3),
    #         panel.grid.minor.x = element_line(color = "gray", linetype = 3),
    #         legend.title.align = 0.5)

    # .target_point <-
    #   lift_table %>%
    #   filter(lift > 1) %>%
    #   group_by(model_id) %>%
    #   filter(lift == min(lift)) %>%
    #   select(model_id, cumulative_data_fraction)

    inform("Lift Plot")
    lift_plot <-
      lift_table %>%
      ggplot() +
      aes(x = cumulative_data_fraction, y = cumulative_lift) +
      geom_line(size = 1, color = "red") +
      geom_hline(aes(yintercept = 1)) +
      # geom_vline(aes(xintercept = .target_point), linetype = "dashed", color = "gray") +
      scale_y_continuous(limits = c(0, NA)) +
      facet_wrap(vars(model_id), scale = "free_y") +
      labs(x = "Population Percent",
           y = NULL,
           color = "Model",
           title = "Cumulative Lift") +
      .plot_theme


    # | Shutdown ----
    # h2o.shutdown(prompt = FALSE)

    # | Results ----
    inform("End Score Function")
    lst(lift_table,
        lift_plot,
        score)
  }


# Test --------------------------------------------------------------------


# data <- NULL
# model <- run_model$metadata_results
# path_data <- "data/train/2018-09-26_holdout_data.rds"
# # path_model <- "data/model/2018-09-27_model.rds"
# path_model <- NULL
# col_id <- "rowid"
#
# test <-
#   demo_score(
#     data = data,
#     model = model,
#     path_data = path_data,
#     path_model = path_model,
#     col_id = col_id)

