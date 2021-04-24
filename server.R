server <- function(input, output, session){
  
  # upload and return information about a csv file
  dat <- reactive({
    req(input$file1)
    if (is.null(input$file1$datapath))
      return(NULL)
    ext <- tools::file_ext(input$file1$datapath)
    validate(
      need(ext %in% c("text/csv",
                      "text/comma-separated-values",
                      "text/tab-separated-values",
                      "text/plain",
                      "txt",
                      "csv", 
                      "tsv"), 
           "Data format not recognised. Please upload a csv file."))
    read.csv(input$file1$datapath, header = input$header, sep = input$sep, 
             check.names = FALSE)
  })
  
  file_name <- reactive({
    req(input$file1)
    paste("File uploaded:")
  })
  
  output$f_name <- renderText({file_name()})
  
  output$file <- renderTable({input$file1})
  
  file_str <- reactive({
    req(input$file1)
    paste("The structure of your data:")
  })
  
  output$str_t <- renderText({file_str()})
  
  output$structure <- renderPrint({str(dat(),
                                       give.head = TRUE, 
                                       give.length = FALSE)})
  
  ################################################
  ## distributions
  ################################################
  
  # one film
  output$select_one_d <- renderUI({
    selectInput("select_one_d", "Select a film:", c(Choose = '', as.list(colnames(dat()))), 
                multiple = FALSE, selectize = FALSE) 
  })
  
  one_d <- reactive({input$select_one_d})
  
  # five-number summary
  one_d_sumTable <- eventReactive(input$runScript_one_d, {
    req(input$select_one_d)
    df <- dat() %>% select(one_d()) %>%
      pivot_longer(cols = 1:length(one_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    dt <- sl_sum(df)
  })
  
  output$one_d_summary <- renderTable({print(one_d_sumTable())})
  
  # re-arrange data and pass to plotting functions
  one_d_boxkde <- reactive({
    req(input$select_one_d)
    df <-dat() %>% select(one_d()) %>%
      pivot_longer(cols = 1:length(one_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    adjkde_plot(df)
  })
  
  one_d_ecdf <- reactive({
    req(input$select_one_d)
    df <-dat() %>% select(one_d()) %>%
      pivot_longer(cols = 1:length(one_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    ecdf_plot(df)
  })
  
  one_d_hist <- reactive({
    req(input$select_one_d)
    df <-dat() %>% select(one_d()) %>%
      pivot_longer(cols = 1:length(one_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    hist_plot(df)
  })
  
  # select plot, output, and download
  one_d_viz <- eventReactive(input$runScript_one_d, {
    switch(input$one_d_plot_type,
           "Adjusted boxplot + kernel density" = one_d_boxkde(),
           "Empirical cumulative distribution function" = one_d_ecdf(),
           "Histogram" = one_d_hist()
    )
  })
  
  output$one_d_plot <- renderPlot({one_d_viz()})
  
  output$one_d_plot_download <- downloadHandler(
    filename = function() {paste(input$select_one_d, "-", input$one_d_plot_type, ".pdf", sep="")},
    content = function(file) {
      ggsave(file, plot = one_d_viz(), device = "pdf", dpi = 600,
             width = 15.92, height = 10.2, units = "cm")}
  )
  
  # two films
  output$select_two_d <- renderUI({
    selectizeInput("select_two_d", "Select two films:", c(Choose = '', as.list(colnames(dat()))), 
                   multiple = TRUE, options = list(maxItems = 2)) 
  })
  
  two_d <- reactive({input$select_two_d})
  
  two_d_sumTable <- eventReactive(input$runScript_two_d, {
    req(input$select_two_d)
    df <- dat() %>% select(two_d()) %>%
      pivot_longer(cols = 1:length(two_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    dt <- sl_sum(df)
  })
  
  output$two_d_summary <- renderTable({(two_d_sumTable())})
  
  two_d_diff <- eventReactive(input$runScript_two_d, {
    req(input$select_two_d)
    df <- dat() %>% select(two_d()) %>%
      pivot_longer(cols = 1:length(two_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    dt <- dom_tab(df)
    })
  
  output$two_d_hld <- renderTable(two_d_diff(), align = c("llrr"))
  
  two_d_adjbox <- reactive({
    req(input$select_two_d)
    df <- dat() %>% select(two_d()) %>%
      pivot_longer(cols = 1:length(two_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    adjbox_M_plot(df)})
  
  two_d_ecdf <- reactive({
    req(input$select_two_d)
    df <- dat() %>% select(two_d()) %>%
      pivot_longer(cols = 1:length(two_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    ecdf_plot(df)})
  
  two_d_kde <- reactive({
    req(input$select_two_d)
    df <- dat() %>% select(two_d()) %>%
      pivot_longer(cols = 1:length(two_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    kde_M_plot(df)})
  
  two_d_quan <- reactive({
    req(input$select_two_d)
    df <- dat() %>% select(two_d()) %>%
      pivot_longer(cols = 1:length(two_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    quan_2_plot(df)})
  
  two_d_viz <- eventReactive(input$runScript_two_d, {
    switch(input$two_d_plot_type,
           "Adjusted boxplot" = two_d_adjbox(),
           "Empirical cumulative distribution function" = two_d_ecdf(),
           "Kernel density" = two_d_kde(),
           "Quantile comparison plot" = two_d_quan()
    )
  })
  
  output$two_d_plot <- renderPlot({two_d_viz()})
  
  two_d_plot_height <- reactive({
    if_else(input$two_d_plot_type %in% c("Quantile comparison plot"),
            15.92, 10.2)
  })
  
  output$two_d_plot_download <- downloadHandler(
    filename = function() {paste(input$two_d_plot_type, ".pdf", sep="")},
    content = function(file) {
      ggsave(file, plot = two_d_viz(), device = "pdf", dpi = 600,
             width = 15.92, height = two_d_plot_height(), units = "cm")}
  )
  
  # three to six films
  output$select_multi_d <- renderUI({
    selectizeInput("select_multi_d", "Select three to six films:", c(Choose = '', as.list(colnames(dat()))), 
                   multiple = TRUE, options = list(maxItems = 6)) 
  })
  
  multi_d <- reactive({input$select_multi_d})
  
  multi_d_sumTable <- eventReactive(input$runScript_multi_d, {
    req(input$select_multi_d)
    df <- dat() %>% select(multi_d()) %>%
      pivot_longer(cols = 1:length(multi_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    dt <- sl_sum(df)
  })
  
  output$multi_d_summary <- renderTable({(multi_d_sumTable())})
  
  multi_d_domTable <- eventReactive(input$runScript_multi_d, {
    req(input$select_multi_d)
    df <- dat() %>% select(multi_d()) %>%
      pivot_longer(cols = 1:length(multi_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    dt <- dom_tab(df)
  })
  
  output$multi_d_dom <- renderTable(multi_d_domTable(), align = c("llrr"))
  
  multi_d_adjbox <- reactive({
    req(input$select_multi_d)
    df <- dat() %>% select(multi_d()) %>%
      pivot_longer(cols = 1:length(multi_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    adjbox_M_plot(df)})
  
  multi_d_ecdf <- reactive({
    req(input$select_multi_d)
    df <- dat() %>% select(multi_d()) %>%
      pivot_longer(cols = 1:length(multi_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    ecdf_plot(df)})
  
  multi_d_kde <- reactive({
    req(input$select_multi_d)
    df <- dat() %>% select(multi_d()) %>%
      pivot_longer(cols = 1:length(multi_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    kde_M_plot(df)})
  
  multi_d_quan <- reactive({
    req(input$select_multi_d)
    df <- dat() %>% select(multi_d()) %>%
      pivot_longer(cols = 1:length(multi_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    quan_M_plot(df)
  })
  
  multi_d_viz <- eventReactive(input$runScript_multi_d, {
    switch(input$multi_d_plot_type,
           "Adjusted boxplot" = multi_d_adjbox(),
           "Empirical cumulative distribution function" = multi_d_ecdf(),
           "Kernel density" = multi_d_kde(),
           "Quantile comparison plot" = multi_d_quan()
    )
  })
  
  output$multi_d_plot <- renderPlot({multi_d_viz()})
  
  multi_d_plot_height <- reactive({
    if_else(input$multi_d_plot_type %in% c("Empirical cumulative distribution function", "Quantile comparison plot"),
            10.2, 21)
  })
  
  output$multi_d_plot_download <- downloadHandler(
    filename = function() {paste(input$multi_d_plot_type, ".pdf", sep="")},
    content = function(file) {
      ggsave(file, plot = multi_d_viz(), device = "pdf", dpi = 600,
             width = 15.92, height = multi_d_plot_height(), units = "cm")}
  )
  
  # group comparison
  output$select_group_one_d <- renderUI({
    selectizeInput("select_group_one_d", "Select films:", c(Choose = '', as.list(colnames(dat()))), 
                multiple = TRUE) 
  })
  
  group_one_d <- reactive({input$select_group_one_d})
  
  group_one_id <- reactive({input$group_one_name})
  
  output$select_group_two_d <- renderUI({
    selectizeInput("select_group_two_d", "Select films:", c(Choose = '', as.list(colnames(dat() %>% select(-c(group_one_d()))))), 
                multiple = TRUE)
  })
  
  group_two_d <- reactive({input$select_group_two_d})
  
  group_two_id <- reactive({input$group_two_name})
  
  group_d_sumTable <- eventReactive(input$runScript_group_d, {
    req(input$select_group_one_d, input$select_group_two_d)
    df_one <- dat() %>% select(group_one_d()) %>%
      pivot_longer(cols = 1:length(group_one_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE) %>%
      mutate(Group = rep(group_one_id(), length(SL)))
    df_two <- dat() %>% select(group_two_d()) %>%
      pivot_longer(cols = 1:length(group_two_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE) %>%
      mutate(Group = rep(group_two_id(), length(SL)))
    group_df <- rbind.data.frame(df_one, df_two)
    dt <- group_sl_sum(group_df)
  })
  
  output$group_d_summary <- renderTable({group_d_sumTable()})
  
  cliff_heatmap <- reactive({
    req(input$select_group_two_d)
    df_one <- dat() %>% select(group_one_d()) %>%
      pivot_longer(cols = 1:length(group_one_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    df_two <- dat() %>% select(group_two_d()) %>%
      pivot_longer(cols = 1:length(group_two_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    cd_heatmap(df_one, df_two, label_x = group_one_id(), label_y = group_two_id())
  })
  
  hl_heatmap <- reactive({
    req(input$select_group_two_d)
    df_one <- dat() %>% select(group_one_d()) %>%
      pivot_longer(cols = 1:length(group_one_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    df_two <- dat() %>% select(group_two_d()) %>%
      pivot_longer(cols = 1:length(group_two_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    hld_heatmap(df_one, df_two, label_x = group_one_id(), label_y = group_two_id())
  })
  
  group_d_ecdf <- reactive({
    req(input$select_group_one_d, input$select_group_two_d)
    df_one <- dat() %>% select(group_one_d()) %>%
      pivot_longer(cols = 1:length(group_one_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE) %>%
      mutate(Group = rep(group_one_id(), length(SL)))
    df_two <- dat() %>% select(group_two_d()) %>%
      pivot_longer(cols = 1:length(group_two_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE) %>%
      mutate(Group = rep(group_two_id(), length(SL)))
    group_df <- rbind.data.frame(df_one, df_two)
    ecdf_group_plot(group_df)
  })
  
  group_d_quan_c <- reactive({
    req(input$select_group_one_d, input$select_group_two_d)
    df_one <- dat() %>% select(group_one_d()) %>%
      pivot_longer(cols = 1:length(group_one_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE) %>%
      mutate(Group = rep(group_one_id(), length(SL)))
    df_two <- dat() %>% select(group_two_d()) %>%
      pivot_longer(cols = 1:length(group_two_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE) %>%
      mutate(Group = rep(group_two_id(), length(SL)))
    group_df <- rbind.data.frame(df_one, df_two)
    quan_group_plot(group_df)
  })
  
  group_d_quan_d <- reactive({
    req(input$select_group_one_d, input$select_group_two_d)
    df_one <- dat() %>% select(group_one_d()) %>%
      pivot_longer(cols = 1:length(group_one_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE) 
    df_two <- dat() %>% select(group_two_d()) %>%
      pivot_longer(cols = 1:length(group_two_d()), names_to = "Title", values_to = "SL", values_drop_na = TRUE) 
    quan_group_diff_plot(df_one, df_two)
  })
  
  group_d_viz <- eventReactive(input$runScript_group_d, {
    switch(input$group_d_plot_type,
           "Cliff's d heatmap" = cliff_heatmap(),
           "Empirical cumulative distribution function" = group_d_ecdf(),
           "HLD heatmap" = hl_heatmap(),
           "Quantile comparison plot" = group_d_quan_c(),
           "Quantile difference plot" = group_d_quan_d()
    )
  })
  
  output$group_d_plot <- renderPlot({group_d_viz()})
  
  output$group_d_plot_download <- downloadHandler(
    filename = function() {paste(input$group_d_plot_type, ".pdf", sep="")},
    content = function(file) {
      ggsave(file, plot = group_d_viz(), device = "pdf", dpi = 600,
             width = 15.92, height = 15.92, units = "cm")}
  )
  
  output$group_d_table_download <- downloadHandler(
    filename = function() {paste(input$group_one_name, " - ", input$group_two_name, ".csv", sep="")},
    content = function(file) {write.csv(group_d_sumTable(), file, row.names = FALSE)}
  )
  
  ################################################
  # time series
  ################################################
  # one film
  output$select_one_ts <- renderUI({
    selectInput("select_one_ts", "Select a film:", c(Choose = '', as.list(colnames(dat()))), 
                multiple = FALSE, selectize = FALSE) 
  })
  
  one_ts <- reactive({input$select_one_ts})
  
  one_ts_countp <- reactive({
    req(input$select_one_ts)
    df <-dat() %>% select(one_ts()) %>%
      pivot_longer(cols = 1:length(one_ts()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    spp_plot(df)
  })
  
  one_ts_cutd <- reactive({
    req(input$select_one_ts)
    df <-dat() %>% select(one_ts()) %>%
      pivot_longer(cols = 1:length(one_ts()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    cutd_plot(df)
  })
  
  one_ts_mls <- reactive({
    req(input$select_one_ts)
    df <-dat() %>% select(one_ts()) %>%
      pivot_longer(cols = 1:length(one_ts()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    loessggplot(df)
  })
  
  one_ts_viz <- eventReactive(input$runScript_one_ts, {
    switch(input$one_ts_plot_type,
           "Counting process" = one_ts_countp(),
           "Cut density" = one_ts_cutd(),
           "Multiple loess smoothers" = one_ts_mls()
    )
  })
  
  output$one_ts_plot <- renderPlot({one_ts_viz()})
  
  output$one_ts_plot_download <- downloadHandler(
    filename = function() {paste(input$select_one_ts, "-", input$one_ts_plot_type, ".pdf", sep="")},
    content = function(file) {
      ggsave(file, plot = one_ts_viz(), device = "pdf", dpi = 600,
             width = 15.92, height = 10.2, units = "cm")}
  )
  
  # two films
  output$select_two_ts <- renderUI({
    selectizeInput("select_two_ts", "Select two films:", c(Choose = '', as.list(colnames(dat()))), 
                   multiple = TRUE, options = list(maxItems = 2)) 
  })
  
  two_ts <- reactive({input$select_two_ts})
  
  two_ts_cutd <- reactive({
    req(input$select_two_ts)
    df <- dat() %>% select(two_ts()) %>%
      pivot_longer(cols = 1:length(two_ts()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    kde_ts_plot(df)
  })
  
  two_ts_count <- reactive({
    req(input$select_two_ts)
    df <- dat() %>% select(two_ts()) %>%
      pivot_longer(cols = 1:length(two_ts()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    countp_M_plot(df)
  })
  
  two_ts_viz <- eventReactive(input$runScript_two_ts, {
    switch(input$two_ts_plot_type,
           "Counting process" = two_ts_count(),
           "Cut density" = two_ts_cutd()
    )
  })
  
  output$two_ts_plot <- renderPlot({two_ts_viz()})
  
  output$two_ts_plot_download <- downloadHandler(
    filename = function() {paste(input$two_ts_plot_type, ".pdf", sep="")},
    content = function(file) {
      ggsave(file, plot = two_ts_viz(), device = "pdf", dpi = 600,
             width = 15.92, height = 10.2, units = "cm")}
  )
  
  # three to six films
  output$select_multi_ts <- renderUI({
    selectizeInput("select_multi_ts", "Select two films:", c(Choose = '', as.list(colnames(dat()))), 
                   multiple = TRUE, options = list(maxItems = 6)) 
  })
  
  multi_ts <- reactive({input$select_multi_ts})
  
  multi_ts_cutd <- reactive({
    req(input$select_multi_ts)
    df <- dat() %>% select(multi_ts()) %>%
      pivot_longer(cols = 1:length(multi_ts()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    kde_ts_plot(df)
  })
  
  multi_ts_count <- reactive({
    req(input$select_multi_ts)
    df <- dat() %>% select(multi_ts()) %>%
      pivot_longer(cols = 1:length(multi_ts()), names_to = "Title", values_to = "SL", values_drop_na = TRUE)
    countp_M_plot(df)
  })
  
  multi_ts_viz <- eventReactive(input$runScript_multi_ts, {
    switch(input$multi_ts_plot_type,
           "Counting process" = multi_ts_count(),
           "Cut density" = multi_ts_cutd()
    )
  })
  
  output$multi_ts_plot <- renderPlot({multi_ts_viz()})
  
  multi_ts_plot_height <- reactive({
    if_else(input$multi_ts_plot_type %in% c("Counting process"),
            15.92, 21)
  })
  
  output$multi_ts_plot_download <- downloadHandler(
    filename = function() {paste(input$multi_ts_plot_type, ".pdf", sep="")},
    content = function(file) {
      ggsave(file, plot = multi_ts_viz(), device = "pdf", dpi = 600,
             width = 15.92, height = multi_ts_plot_height(), units = "cm")}
  )
  
  # Two groups of films
  output$select_group_one_ts <- renderUI({
    selectizeInput("select_group_one_ts", "Select films:", c(Choose = '', as.list(colnames(dat()))), 
                   multiple = TRUE) 
  })
  
  group_one_ts <- reactive({input$select_group_one_ts})
  
  group_one_id_ts <- reactive({input$group_one_name_ts})
  
  output$select_group_two_ts <- renderUI({
    selectizeInput("select_group_two_ts", "Select films:", c(Choose = '', as.list(colnames(dat() %>% select(-c(group_one_ts()))))), 
                   multiple = TRUE)
  })
  
  group_two_ts <- reactive({input$select_group_two_ts})
  
  group_two_id_ts <- reactive({input$group_two_name_ts})
  
  
  group_ts_count <- reactive({
    req(input$select_group_one_ts, input$select_group_two_ts)
    df_one <- dat() %>% select(group_one_ts()) %>%
      pivot_longer(cols = 1:length(group_one_ts()), names_to = "Title", values_to = "SL", values_drop_na = TRUE) %>%
      mutate(Group = rep(group_one_id_ts(), length(SL)))
    df_two <- dat() %>% select(group_two_ts()) %>%
      pivot_longer(cols = 1:length(group_two_ts()), names_to = "Title", values_to = "SL", values_drop_na = TRUE) %>%
      mutate(Group = rep(group_two_id_ts(), length(SL)))
    group_df <- rbind.data.frame(df_one, df_two)
    countp_group_plot(group_df)
  })
  
  group_ts_viz <- eventReactive(input$runScript_group_ts, {
    switch(input$group_ts_plot_type,
           "Counting process" = group_ts_count()
    )
  })
  
  output$group_ts_plot <- renderPlot({group_ts_viz()})
  
  output$group_ts_plot_download <- downloadHandler(
    filename = function() {paste(input$group_ts_plot_type, ".pdf", sep="")},
    content = function(file) {
      ggsave(file, plot = group_ts_viz(), device = "pdf", dpi = 600,
             width = 15.92, height = 15.92, units = "cm")}
  )
  
}