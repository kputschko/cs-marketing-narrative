
# Plot Theme --------------------------------------------------------------

.plot_theme <-
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0, face = "bold"),
        strip.text.x = element_text(face = "bold"),
        panel.background = element_rect(color = "gray"),
        panel.grid.major.x = element_line(color = "gray", linetype = 3),
        panel.grid.minor.x = element_line(color = "gray", linetype = 3),
        legend.title.align = 0.5)

# As Formula --------------------------------------------------------------

.fx_as_formula <- function(y, x) {

  require(dplyr)
  require(stringr)

  work_table <-
    tibble(y, x = list(x)) %>%
    mutate(x_list = map2(x, y, ~ setdiff(.x, .y)),
           x_string = map_chr(x_list, ~ str_c(.x, collapse = "+")),
           formula = str_glue("{y} ~ {x_string}"))

  if (nrow(work_table) == 1) {

    work_table %>%
      pull(formula) %>%
      map(as.formula) %>%
      pluck(1)

  } else {

    work_table %>%
      pull(formula) %>%
      map(as.formula)

  }

}


# Top Percent -------------------------------------------------------------

.fx_top_p <- function(.data, .p, .wt) {

  library(tidyverse)

  n <- nrow(.data)
  p <- ceiling(n * .p)

  top_n(x = .data, n = p, wt = !!sym(.wt))

}


# Nesting -----------------------------------------------------------------

.fx_nest <- function(data) {

# Use: To prepare for creating many models
# Assumes: Input data is ready for subsetting, and groups have been applied


  require(tidyr)
  require(tibble)

  .groups <-
    data %>%
    group_vars %>%
    length()

  if (.groups == 0) {

    tribble(~group, ~data,
            "NA",   data) %>%
      rowid_to_column("index")

  } else {

    data %>% nest() %>% rowid_to_column("index")

  }

}



# PMAS Manipulate ---------------------------------------------------------

.fx_manipulate <- function(.data,
                           .group = NULL,
                           .mutate = NULL,
                           .filter = NULL,
                           .binary = NULL,
                           .exclude = NULL,
                           .response = NULL,
                           .sample_frac = 1,
                           .strings_to_factor = TRUE) {

  require(dplyr)
  require(purrr)
  require(rlang)
  require(stringr)

  # === Validate Input ===
  if (is.numeric(.sample_frac)) {stopifnot(.sample_frac %>% between(0, 1))}

  if (!is.null(.mutate)) {stopifnot(is_named(.mutate),
                                    is_list(.mutate))}

  if (!is.null(.group))    {stopifnot(is.character(.group))}
  if (!is.null(.binary))   {stopifnot(is.character(.binary))}
  if (!is.null(.filter))   {stopifnot(is.character(.filter))}
  if (!is.null(.response)) {stopifnot(is.character(.response))}


  # === Sample Logic ===
  .sample_frac <- if (is.null(.sample_frac)) { 1L } else {.sample_frac}

  # === Mutate Logic ===
  .data_mutate <- if (is.null(.mutate)) {NULL} else {.mutate %>% map(~parse_quo(.x, env = caller_env()))}

  # === Filter Logic ===
  .data_filter <- if (is.null(.filter)) {NULL} else {.filter %>% map(~parse_quo(.x, env = caller_env()))}

  # === Group Logic ===
  .data_group <- if (is.null(.group)) {NULL} else {syms(.group)}

  # === Exclusion Logic ===
  .data_include <-
    if (is.null(.exclude)) {

      colnames(.data) %>% syms()

    } else {

      colnames(.data) %>% setdiff(.exclude) %>% union(names(.mutate)) %>% syms()

    }

  # === Binary Logic ===
  .data_binary <-
    if (is.null(.binary)) {NULL
    } else {

      binary_values <- str_glue("ifelse({.binary} > 0, 1, 0)") %>% as_list()
      binary_names  <- str_c("bin", .binary, sep = "_")
      binary_list   <- set_names(x = binary_values, nm = binary_names)
      binary_logic  <- map(binary_list, ~parse_quo(.x, env = caller_env()))

    }

  # === Respone na.rm ===
  .data_response_na.rm <-
    if (is.null(.response)) {NULL
    } else {

      str_glue("!is.na({.response})") %>%
        str_c(collapse = " | ") %>%
        map(~parse_quo(.x, env = caller_env()))

    }


  # === Apply Logic ===
  .data_output <-
    .data %>%
    mutate(!!! .data_mutate) %>%
    mutate(!!! .data_binary) %>%
    filter(!!! .data_filter) %>%
    filter(!!! .data_response_na.rm) %>%
    group_by(!!! .data_group) %>%
    select(!!! .data_include)


  # === Sample ===
  if (.sample_frac < 1) {

    .data_output <-
      .data_output %>%
      sample_frac(size = .sample_frac)

  }


  # === Strings to Factor ===
  if (.strings_to_factor) {

    cols_factor <-
      .data_output %>%
      select_if(is.character) %>%
      colnames() %>%
      union(names(.data_binary))

    .data_output <-
      .data_output %>%
      ungroup %>%
      mutate_at(cols_factor, as.factor) %>%
      group_by(!!! .data_group)

  }


  return(.data_output)

}



# Lift Table --------------------------------------------------------------

.fx_lift <- function(data, actual, predicted, n = 20) {

# Assumes at least two columns are present in input: Actual and Predicted probabilities


  require(tidyverse)
  require(rlang)

  if (class(data[[actual]]) == "factor" & n_distinct(data[[actual]]) == 2) {

    data <-
      data %>%
      mutate(!! actual := str_remove(!! sym(actual), 'X') %>% as.integer())

  }

  Overall_Mean_Actual    <- mean(data[[actual]], na.rm = TRUE)
  Overall_Mean_Predicted <- mean(data[[predicted]], na.rm = TRUE)

  Overall_Sum_Actual <- sum(data[[actual]], na.rm = TRUE)
  Overall_Sum_Predicted <- sum(data[[predicted]], na.rm = TRUE)

  Overall_Size <- nrow(data)

  summary_of_predictions <-
    data %>%
    as_tibble() %>%
    rename_("Predicted" = predicted, "Actual" = actual) %>%
    arrange(desc(Predicted), desc(Actual)) %>%
    mutate(Depth = ntile(desc(Predicted), n)) %>%
    group_by(Depth) %>%
    summarise(
      Ptile_N = n(),
      Ptile_Percent = Ptile_N / !! Overall_Size,
      Ptile_Max = max(Predicted),
      Ptile_Min = min(Predicted),
      Ptile_Sum_Actual = sum(Actual, na.rm = TRUE),
      Ptile_Sum_Predicted = sum(Predicted, na.rm = TRUE),
      Ptile_Rate_Actual = Ptile_Sum_Actual / !! Overall_Sum_Actual,
      Ptile_Rate_Predicted = Ptile_Sum_Predicted / !! Overall_Sum_Predicted,
      Ptile_Mean_Actual = mean(Actual),
      Ptile_Mean_Predicted = mean(Predicted),
      Ptile_StDev_Actual = sd(Actual),
      Ptile_StDev_Predicted = sd(Predicted),
      Ptile_Lift_Actual = Ptile_Mean_Actual / !! Overall_Mean_Actual,
      Ptile_Lift_Predicted = Ptile_Mean_Predicted / !! Overall_Mean_Predicted
    )


  collapsing_groups <-
    summary_of_predictions %>%
    group_by(Ptile_Max) %>%
    summarise(
      Ptile_N = sum(Ptile_N),
      Ptile_Percent = sum(Ptile_Percent),
      Ptile_Min = min(Ptile_Min),
      Ptile_Rate_Actual = sum(Ptile_Rate_Actual),
      Ptile_Rate_Predicted = sum(Ptile_Rate_Predicted),
      Ptile_Mean_Actual = mean(Ptile_Mean_Actual),
      Ptile_Mean_Predicted = mean(Ptile_Mean_Predicted),
      Ptile_StDev_Actual = mean(Ptile_StDev_Actual),
      Ptile_StDev_Predicted = mean(Ptile_StDev_Predicted),
      Ptile_Sum_Actual = sum(Ptile_Sum_Actual),
      Ptile_Sum_Predicted = sum(Ptile_Sum_Predicted),
      Ptile_Lift_Actual = mean(Ptile_Lift_Actual),
      Ptile_Lift_Predicted = mean(Ptile_Lift_Predicted)
    )


  final_summary <-
    collapsing_groups %>%
    arrange(desc(Ptile_Max)) %>%
    mutate(
      Depth = sequence(n()),
      Cu_N = cumsum(Ptile_N),
      Cu_Percent = cumsum(Ptile_Percent),
      Cu_Sum_Actual = cumsum(Ptile_Sum_Actual),
      Cu_Sum_Predicted = cumsum(Ptile_Sum_Predicted),
      Cu_Rate_Actual = Cu_Sum_Actual / !! Overall_Sum_Actual,
      Cu_Rate_Predicted = Cu_Sum_Predicted / !! Overall_Sum_Predicted,
      Cu_Mean_Actual = Cu_Sum_Actual / Cu_N,
      Cu_Mean_Predicted = Cu_Sum_Predicted / Cu_N,
      Cu_Lift_Actual = Cu_Mean_Actual / !! Overall_Mean_Actual,
      Cu_Lift_Predicted = Cu_Mean_Predicted / !! Overall_Mean_Predicted,
      Percentile = 1 - round(Cu_Percent, 2),
      Response = !! actual
    )


  final_table <-
    final_summary %>%
    select(
      Depth,
      Percentile,
      Ptile_Min, Ptile_Max,
      Ptile_N, Cu_N,
      Ptile_Percent, Cu_Percent,
      Ptile_Sum_Actual, Cu_Sum_Actual,
      Ptile_Sum_Predicted, Cu_Sum_Predicted,
      Ptile_Rate_Actual, Cu_Rate_Actual,
      Ptile_Rate_Predicted, Cu_Rate_Predicted,
      Ptile_Mean_Actual, Cu_Mean_Actual,
      Ptile_Mean_Predicted, Cu_Mean_Predicted,
      Ptile_StDev_Predicted, Ptile_StDev_Actual,
      Ptile_Lift_Actual, Cu_Lift_Actual,
      Ptile_Lift_Predicted, Cu_Lift_Predicted,
      Response
    )


  return(final_table)

}


# Incremental Lift Plot ---------------------------------------------------

.fx_lift_plot_incremental <- function(lift_table, label = NULL) {

  lift_table %>%
    ggplot(aes(x = Depth, y = Cu_Lift_Actual)) +

    geom_line() +
    geom_point() +
    geom_hline(aes(yintercept = 1), col = 'red') +

    scale_y_continuous(
      name = "Percent Lift") +

    ggtitle(
      label = "Incremental Lift",
      subtitle = paste(label, sep = " - ")) +

    theme_minimal() +
    theme(plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5))

}




# Cumulative Lift Plot ----------------------------------------------------
.fx_lift_plot_cumulative <- function(lift_table, label = NULL){

  lift_table %>%
    rbind(rep(0, 12)) %>%

    ggplot(aes(x = Cu_Percent, y = Cu_Rate_Actual)) +
    geom_line() +
    geom_point() +

    geom_abline(aes(slope = 1, intercept = 0), col = "red") +

    scale_x_continuous(
      minor_breaks = seq(0.05:0.95, by = .10),
      breaks = seq(.10:1, by = .10),
      name = "Percentile Depth") +

    scale_y_continuous(
      name = "Percent Total Response",
      limits = c(0, NA)) +

    ggtitle(label = "Cumulative Lift",
            subtitle = paste(label, sep = " - ")) +

    theme_minimal() +
    theme(plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5))

}



# Describe ----------------------------------------------------------------


.fx_describe <- function(data) {

  require(foreach)
  require(stringr)
  require(rlang)

  stopifnot(is.data.frame(data))


  foreach(i = 1:ncol(data), .combine = "rbind") %do% {

    column_name  <- data %>% colnames() %>% pluck(i)
    column_type  <- data[[column_name]] %>% class()
    column_rlang <- sym(column_name)
    filter_rlang <- parse_quo(x = str_glue("is.na({column_name})"), env = caller_env())

    # n_missing <- data %>% filter(!!! filter_rlang) %>% nrow()

    if (column_type %in% c("factor", "character")) {

      data %>%
        summarise(n = n(),
                  n_distinct = n_distinct(!! column_rlang, na.rm = TRUE),
                  n_missing  = sum(is.na(!! column_rlang))) %>%
        mutate(column_name = !! column_name,
               column_type = !! column_type,
               min = NA,
               med = NA,
               max = NA,
               mean = NA,
               sd = NA
               ) %>%
        select(column_name, column_type,
               n, n_missing, n_distinct,
               min, max, mean, sd)

    } else {

      data %>%
        summarise(n = n(),
                  n_distinct = n_distinct(!! column_rlang, na.rm = TRUE),
                  n_missing  = sum(is.na(!! column_rlang)),
                  mean = mean(!! column_rlang, na.rm = TRUE),
                  sd = sd(!! column_rlang, na.rm = TRUE),
                  min = min(!! column_rlang, na.rm = TRUE),
                  med = median(!! column_rlang, na.rm = TRUE),
                  max = max(!! column_rlang, na.rm = TRUE)) %>%
        mutate(column_name = !! column_name,
               column_type = !! column_type) %>%
        mutate_if(.predicate = is.numeric,
                  .funs = function(x) {format(x, scientific = FALSE, digits = 4)}) %>%
        select(column_name, column_type,
               n, n_missing, n_distinct,
               min, max, mean, sd)


    }



  }

}
