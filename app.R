
# Segmentation: Marketing Perspective -------------------------------------

# Packages
pacman::p_load(
  tidyverse,
  modelr,
  rlang,
  shiny,
  shinyBS,
  shinydashboard,
  scales,
  plotly,
  DT,
  rhandsontable)


source("R/fx_prep.R")
source("R/fx_model.R")
source("R/fx_score.R")
source("R/fx_segment.R")
source("R/fx_utility_functions.R")

inform(str_c("Application launched at ", Sys.time()))

# UI ----------------------------------------------------------------------
ui <-
  dashboardPage(
    header = dashboardHeader(title = "Segment & Target"),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(

      # |- Import ----
      fluidRow(
        box(h4(strong("Import Data")), width = 3,
            hr(),
            selectizeInput(inputId = "import_data_path",
                           label = "Select Raw Data",
                           multiple = TRUE,
                           selected = NULL,
                           choices = dir("data", pattern = ".csv"),
                           options = list(maxItems = 1, placeholder = "NULL")),
            textInput(inputId = "import_data_delim",
                      label = "Data Delimiter",
                      value = ";"),
            hr(),
            actionButton("import_data_button", "Import")
        ),

        tabBox(title = strong("Data Summary"), width = 9,
               tabPanel("Summary", uiOutput("import_info")),
               tabPanel("Details", DTOutput("data_raw"))
        )
      ), # close Import row

      # |- Segment ----
      fluidRow(
        box(h4(strong("Create Segments")), width = 3, hr(),
            uiOutput("segment_cols"),
            radioButtons("segment_search",
                         "Segment Search",
                         selected = TRUE,
                         choiceNames = c("Automatic", "Manual"),
                         choiceValues = c(TRUE, FALSE)),
            numericInput("segment_k", "Number of Segments", value = 20, min = 1, max = 100),
            hr(),
            actionButton("segment_button", label = "Create Segments")
        ),

        tabBox(title = strong("Segment Summary"), width = 9,
               tabPanel("Summary", uiOutput("segment_summary")),
               tabPanel("Details", DTOutput("segment_summary_table")),
               tabPanel("Metrics", verbatimTextOutput("segment_metrics")))
      ), # close Seg row

      # |- Sample ----
      fluidRow(
        box(h4(strong("Initial Campaign")), width = 3, hr(),
            uiOutput("sample_size"),
            hr(),
            actionButton("sample_button", "Send Initial Campaign"),
            footer = em("Initial campaign list is a random sample of users")),

        tabBox(title = strong("Campaign Results"), width = 9,
               # tabPanel("Debug", verbatimTextOutput("sample_debug")),
               tabPanel("Summary", uiOutput("sample_summary")),
               tabPanel("Details", DTOutput("sample_details"))
        )

      ), # close Random Sample row

      # |- Model ----
      fluidRow(
        box(h4(strong("Machine Learning")), width = 3, hr(),
            uiOutput("model_segment_list"),
            uiOutput("model_automl_runtime"),
            hr(),
            actionButton("model_button", "Analyze Data")),
        tabBox(title = strong("ML Recommendations"), width = 9,
               # tabPanel("Debug", verbatimTextOutput("model_debug")),
               tabPanel("Summary",
                        p("Based on the initial campaign responses, the ML model estimates:"),
                        uiOutput("model_recommendation")),
               tabPanel("Cumulative Lift", plotOutput("model_lift")))
      ), # close ML row

      # |- Score ----
      fluidRow(
        box(h4(strong("Target Population")), width = 3, hr(),
            actionButton(inputId = "score_button", label = "Apply ML Model"),
            hr(),
            numericInput("score_target", label = "Target Top Percent", value = 10, min = 0, max = 100, step = 5),
            actionButton("score_target_button", label = "Send Targeted Campaign")),
        tabBox(title = strong("Target Summary"), width = 9,
               # tabPanel("Debug", verbatimTextOutput("score_debug")),
               tabPanel("Summary",
                        h2("Score"),
                        uiOutput("score_summary"), hr(),
                        h2("Estimate"),
                        uiOutput("score_target"), hr(),
                        h2("Send"),
                        uiOutput("score_target_result")),
               tabPanel("Details",
                        DTOutput("score_lift")))
      )

    ) # close dashboard body
  ) # close ui

# Server ------------------------------------------------------------------

server <- function(input, output) {

  # |- Import ----
  data_raw <- eventReactive(input$import_data_button, {
    input$import_data_path %>%
      str_c("data/", .) %>%
      read_delim(input$import_data_delim)
  })

  data_summary <- reactive(data_raw() %>% .fx_describe())
  data_ncol <- reactive(data_raw() %>% ncol())
  data_nrow <- reactive(data_summary()$n %>% unique() %>% parse_integer())
  data_cols <- reactive(data_summary()$column_name)

  output$data_raw <- renderDT({
    data_summary() %>%
      datatable(extensions = c('Scroller'),
                options = list(dom = 't',
                               scrollY = 200,
                               scroller = TRUE,
                               scrollX = TRUE))
  })

  output$import_info <- renderUI({
    str_glue("File <i>{input$import_data_path}</i> imported with {data_nrow() %>% comma()} rows and {data_ncol() %>% comma()} columns.") %>%
      HTML()

  })

  # |- Segment ----
  output$segment_cols <- renderUI(
    selectizeInput("segment_cols",
                   label = "Segmentation Columns",
                   choices = data_cols(),
                   selected = NULL,
                   multiple = TRUE,
                   options = list(placeholder = 'All'))
  )

  segment_results <- eventReactive(input$segment_button, {

    segment_model <-
      data_raw() %>%
      demo_segment(k = input$segment_k,
                   estimate_k = input$segment_search %>% parse_expr(),
                   cols_include = input$segment_cols)

    segment_data <-
      data_raw() %>%
      add_column(segment = factor(segment_model$assignment))

    summary_table  <- segment_model$summary
    summary_metric <- segment_model$model@model$training_metrics
    summary_cols <- ifelse(input$segment_cols %>% is_empty(), ncol(segment_data) - 1, length(input$segment_cols))
    summary_string <- str_glue("{nrow(summary_table)} segments created using {summary_cols} columns.")

    lst(segment_model,
        segment_data,
        summary_metric,
        summary_table,
        summary_string)

  })

  output$segment_summary <- renderUI({segment_results()$summary_string %>% HTML()})
  output$segment_metrics <- renderPrint(segment_results() %>% pluck("summary_metric"))

  output$segment_summary_table <- renderDT({
    segment_results() %>%
      pluck("summary_table") %>%
      mutate_if(is_numeric, scales::number_format(0.001, big.mark = ",")) %>%
      datatable(extensions = c('Scroller'), options = list(dom = 't', scrollY = 300, scroller = TRUE, scrollX = TRUE))
  })

  data_segment <- reactive(segment_results()$segment_data)



  # |- Sample ----
  output$sample_size <- renderUI({
    sliderInput(inputId = "sample_size",
                label = "Select Size of Initial Campaign",
                value = 10000,
                min = 0,
                max = data_nrow(),
                step = 500)
  })


  data_sample_list <-
    eventReactive(input$sample_button, {

      sample_proportion <- input$sample_size / data_nrow()

      sample_list <-
        data_segment() %>%
        resample_partition(p = c(initial = sample_proportion,
                                 holdout = 1 - sample_proportion))

      lst(
        data_initial = sample_list$initial %>% as_data_frame(),
        data_holdout = sample_list$holdout %>% as_data_frame(),
        initial_summary = data_initial %>% count(y, segment) %>% mutate(prop = n / sum(n))
      )
    })

  output$sample_summary <- renderUI({
    nrow <- data_sample_list()$initial_summary$n %>% sum() %>% comma()
    prop <- data_sample_list()$initial_summary %>% filter(y == "yes") %>% summarise_at("prop", sum) %>% pull() %>% percent(accuracy = 0.01)
    str_glue("Initial campaign sent to <i>{nrow} users</i>.<br><br>
             <i>One Week Later...</i><br><br>
             Initial campaign had a <b>{prop}</b> response rate.") %>% HTML()
  })

  output$sample_details <- renderDT({
    data_sample_list() %>%
      pluck("initial_summary") %>%
      mutate_at("prop", percent_format(accuracy = 0.0001)) %>%
      datatable(extensions = c('Scroller'), rownames = FALSE,
                options = list(dom = 't',
                               scrollY = 250,
                               scroller = TRUE,
                               scrollX = TRUE))
  })

  output$sample_debug <- renderPrint({
    lst(
      button = input$sample_button,
      rows = data_segment() %>% nrow(),
      cols = data_segment() %>% ncol(),
      class = data_segment() %>% class(),
      size = input$sample_size,
      nrow_ = try(data_nrow()),
      initial = try(output$sample_summary())
    )

  })

  # |- Model ----

  # UI - List Segments to Include in Model
  output$model_segment_list <- renderUI(
    selectizeInput("model_segment_selection",
                   label = "Filter by Segment",
                   multiple = TRUE,
                   selected = NULL,
                   choices = data_sample_list()$data_initial %>% distinct(segment) %>% pull() %>% sort(),
                   options = list(placeholder = "All"))
  )

  # UI - Input Max Runtime for Model
  output$model_automl_runtime <- renderUI(
    numericInput(inputId = "model_automl_runtime",
                 label = "Auto ML Max Runtime",
                 min = 0,
                 step = 5,
                 max = 600,
                 value = ifelse(input$model_segment_selection %>% is_null(), 120,
                                input$model_segment_selection %>% length() %>% magrittr::multiply_by(30)))
  )

  # Push the Button - Build the Model
  model_automl <-
    eventReactive(input$model_button, {

      model_filter <-
        if (input$model_segment_selection %>% is_empty()) {
          NULL
        } else {
          "segment %in% input$model_segment_selection" %>% parse_expr()}

      data_sample_list() %>%
        pluck("data_initial") %>%
        filter(!!! model_filter) %>%
        mutate(y = ifelse(y == "yes", 1, 0)) %>%
        # demo_model(data = ., response = "y", algorithms = "auto")
        demo_model(data = ., response = "y", algorithms = "lr")

    })

  output$model_lift <- renderPlot({
    model_automl() %>%
      pluck("metadata_plots") %>%
      deframe() %>%
      pluck("plot_lift")
  })

  output$model_recommendation <- renderUI({

    model_automl() %>%
      pluck("metadata_results") %>%
      pull("lift") %>%
      flatten_df() %>%
      mutate(
        f_cdf = cumulative_data_fraction %>% percent(accuracy = 0.01),
        f_cl  = cumulative_lift %>% number(accuracy = 0.01),
        rec = str_glue("- Targeting the top {f_cdf} is estimated to have {f_cl} times more responses than a random {f_cdf}<br>")) %>%
      slice(c(5, 6, 8, 10)) %>%
      pull(rec) %>%
      HTML()


  })


  output$model_debug <- renderPrint({

    lst(select = input$model_segment_selection,
        sellen = input$model_segment_selection %>% length(),
        selchk = input$model_segment_selection %>% is_empty(),
        runtime = input$model_automl_runtime,
        data = data_sample_list()$data_initial %>% distinct(segment) %>% pull() %>% sort(),
        model = model_automl() %>% names()
        )

  })


  # |- Score ----

  score_results <- eventReactive(input$score_button, {

    score_filter <-
      if (input$model_segment_selection %>% is_empty()) {
        NULL
      } else {
        "segment %in% input$model_segment_selection" %>% parse_expr()}

    data_score <-
      data_sample_list() %>%
      pluck("data_holdout") %>%
      filter(!!! score_filter) %>%
      mutate(y = ifelse(y == "yes", 1, 0)) %>%
      rowid_to_column("ID")

    data_y <- data_score %>% select(ID, y, segment)
    data_x <- data_score %>% select(-y)

    model_table <- model_automl()$metadata_results

    score <- demo_score(data = data_x, model = model_table, col_id = "ID")

    score_list <- score %>% pluck("score") %>% left_join(data_y)
    score_nrow <- score_list %>% nrow()
    score_lift <- score %>% pluck("lift_table")

    lst(score_list,
        score_nrow,
        score_lift)


  })

  output$score_lift <- renderDT({
    score_results() %>%
      pluck("score_lift") %>%
      select(cumulative_data_fraction, cumulative_n, cumulative_lift) %>%
      mutate(cumulative_data_fraction = cumulative_data_fraction %>% percent(),
             cumulative_n = cumulative_n %>% comma(),
             cumulative_lift = cumulative_lift %>% number(accuracy = 0.01)) %>%
      datatable(extensions = c('Scroller'), rownames = FALSE,
                options = list(dom = 't', scrollY = 200, scroller = TRUE, scrollX = TRUE))
  })

  output$score_summary <- renderUI({
    str_glue("Scoring completed on population of {score_results()$score_nrow %>% comma()} users.") %>%
      HTML()
  })

  output$score_target <- renderUI({
    score_results() %>%
      pluck("score_lift") %>%
      mutate(match_cdf = percent(cumulative_data_fraction, accuracy = 1)) %>%
      filter(match_cdf == number(input$score_target, suffix = "%")) %>%
      mutate(string = str_glue("If we target the top {match_cdf} of the population, we will send the campaign to {cumulative_n %>% comma()} users,<br>
                               and can expect {cumulative_lift %>% number(accuracy = 0.01)} times as many responses as sending to a random {match_cdf}")) %>%
      pull(string) %>%
      HTML()

  })

  score_target_send <-
    eventReactive(input$score_target_button, {

      score_n  <- score_results()$score_nrow
      target_p <- input$score_target / 100
      target_n <- floor(target_p * score_n)

      score_results()$score_list %>%
        top_n(n = target_n, wt = p1) %>%
        summarise(total = length(y),
                  responses = sum(y),
                  response_rate = mean(y))
    })


  output$score_target_result <- renderUI({
    t_total    <- score_target_send()$total %>% comma()
    t_response <- score_target_send()$responses %>% comma()
    t_rate     <- score_target_send()$response_rate %>% percent()

    str_glue("Sending targeted campaign to {t_total} users.<br><br>
             <i>One week later...</i><br><br>
             {t_response} users responded to the targeted campaign,
             giving us a response rate of <b>{t_rate}</b>.") %>%
      HTML()

  })


  output$score_debug <- renderPrint(
    lst(button = input$score_button,
        # debug_score = score_results(),
        target_send = score_target_send()
    )
  )

}

# Run ---------------------------------------------------------------------

shinyApp(ui = ui, server = server)

