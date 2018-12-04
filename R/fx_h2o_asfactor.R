
# H2O: Convert Strings to Factors -----------------------------------------

fx_h2o_asfactor <- function(data_h2o, cols_to_factor, key = "data_factor") {

  library(foreach)

  stopifnot("H2OFrame" %in% class(data_h2o),
            length(cols_to_factor) > 0)

  cols_to_ignore <- setdiff(h2o.colnames(data_h2o), cols_to_factor)

  .factor_individual_loop <-
    foreach(i = seq_along(cols_to_factor), .errorhandling = "pass") %do% {
      .loop_col <- cols_to_factor[i]
      .loop_key <- str_c("factor", .loop_col, sep = "_")
      .loop_factor <- h2o.asfactor(data_h2o[[.loop_col]])
      .loop_factor %>% h2o.assign(.loop_key)
      .loop_factor %>% h2o.rm()
      .loop_key %>% h2o.getFrame()
    }

  .factor_individual <-
    .factor_individual_loop %>%
    keep(~ class(.) == "H2OFrame")

  if (.factor_individual %>% is_empty()) abort("Factors did not convert sucessfully!")

  .factor_combine <-
    data_h2o[ , cols_to_ignore] %>%
    h2o.cbind(.factor_individual)

  .factor_full <- h2o.assign(.factor_combine, key = key)
  h2o.rm(.factor_combine)

  .factor_full


}


# Test --------------------------------------------------------------------

# h2o.init()
#
# data_h2o <- mtcars %>% rownames_to_column() %>% as.h2o()
# cols_to_factor <- "rowname"
# key <- "data_factor"
#
# data_h2o %>% fx_h2o_asfactor(cols_to_factor, key)
