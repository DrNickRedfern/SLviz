time_series <- tabPanel(
  "Time series",
  
  titlePanel("Visualise editing as a time series"),
  
  tabsetPanel(type = "tabs",
              tabPanel("One Film",
                       fluidRow(style = "padding-top: 12px", 
                                sidebarLayout(
                                  column(4,
                                         sidebarPanel(
                                           width = 12,
                                           # select data set
                                           uiOutput("select_one_ts"),
                                           
                                           # Horizontal line
                                           tags$hr(style = "border-color: purple;"),
                                           
                                           # select plot type
                                           selectInput("one_ts_plot_type", "Select a visualisation:", 
                                                       choices = c(Choose = '', "Counting process", 
                                                                   "Cut density", 
                                                                   "Multiple loess smoothers")),
                                           # add action button
                                           actionButton("runScript_one_ts", "Plot time series"),
                                           
                                           # Horizontal line
                                           tags$hr(style = "border-color: purple;"),
                                           
                                           # download button
                                           downloadButton("one_ts_plot_download", label = "Download plot as pdf file")
                                           )
                                         ),
                                  column(8, 
                                         mainPanel(
                                           width = 12,
                                           plotOutput("one_ts_plot") %>% withSpinner(color = "#008080")
                                         )
                                         )
                                  )
                       )
              ),
              tabPanel("Two films",
                       fluidRow(style = "padding-top: 12px", 
                                sidebarLayout(
                                  column(4,
                                         sidebarPanel(
                                           width = 12,
                                           # select data set
                                           uiOutput("select_two_ts"),
                                           
                                           # Horizontal line
                                           tags$hr(style = "border-color: purple;"),
                                           
                                           # select plt type
                                           selectInput("two_ts_plot_type", "Select a visualisation:", 
                                                       choices = c(Choose = '', "Counting process", 
                                                                   "Cut density"
                                                       )
                                           ),
                                           # add action button
                                           actionButton("runScript_two_ts", "Plot time series"),
                                           
                                           # Horizontal line
                                           tags$hr(style = "border-color: purple;"),
                                           downloadButton("two_ts_plot_download", label = "Download plot as pdf file")
                                           )
                                         ),
                                  column(8, 
                                         mainPanel(
                                           width = 12,
                                           plotOutput("two_ts_plot") %>% withSpinner(color = "#008080")
                                         )
                                  )
                                )
                       )
              ),
              tabPanel("Three to six films",
                       fluidRow(style = "padding-top: 12px", 
                                sidebarLayout(
                                  column(4,
                                         sidebarPanel(
                                           width = 12,
                                           # select data set
                                           uiOutput("select_multi_ts"),
                                           
                                           # Horizontal line
                                           tags$hr(style = "border-color: purple;"),
                                           
                                           # select plot type
                                           selectInput("multi_ts_plot_type", "Select a visualisation:", 
                                                       choices = c(Choose = '', "Counting process", 
                                                                   "Cut density"
                                                       )
                                           ),
                                           # add action button
                                           actionButton("runScript_multi_ts", "Plot time series"),
                                           tags$hr(style = "border-color: purple;"),
                                           downloadButton("multi_ts_plot_download", label = "Download plot as pdf file")
                                           )
                                         ),
                                  column(8, 
                                         mainPanel(
                                           width = 12,
                                           plotOutput("multi_ts_plot", width = "100%", height = "670px") %>% withSpinner(color = "#008080")
                                         )
                                  )
                                )
                       )
              ),
              tabPanel("Two groups of films",
                       fluidRow(style = "padding-top: 12px",
                                sidebarLayout(
                                  column(4,
                                         sidebarPanel(
                                           width = 12,
                                           p("If group names are not provided, the default names are used."),
                                           uiOutput("select_group_one_ts"),
                                           textInput("group_one_name_ts", label = "Enter a name for group A", value = "Group A"), 
                                           tags$hr(style = "border-color: #008080"),
                                           uiOutput("select_group_two_ts"),
                                           textInput("group_two_name_ts", label = "Enter a name for group B", value = "Group B"),
                                           tags$hr(style = "border-color: purple;"),
                                           selectInput("group_ts_plot_type", "Select a visualisation:", 
                                                       choices = c(Choose = '', "Counting process"
                                                       )
                                           ),
                                           # add action button
                                           actionButton("runScript_group_ts", "Plot time series"),
                                           tags$hr(style = "border-color: purple;"),
                                           downloadButton("group_ts_plot_download", label = "Download plot as pdf file")
                                         )
                                  ),
                                  column(8,
                                         mainPanel(
                                           width = 12,
                                           plotOutput("group_ts_plot", width = "100%", height = "670px") %>% withSpinner(color = "#008080")
                                         )
                                  )
                                )
                       )
              )
  )

)