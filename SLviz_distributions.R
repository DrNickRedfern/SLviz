distributions <- tabPanel(
  "Distributions",
  
  titlePanel("Summarise and plot shot length distributions"),
  
  tabsetPanel(type = "tabs",
              tabPanel("One Film",
                       fluidRow(style = "padding-top: 12px", 
                                sidebarLayout(
                                  column(4,
                                         sidebarPanel(
                                           width = 12,
                                           # select data set
                                           uiOutput("select_one_d"),
                                           # Horizontal line
                                           tags$hr(style = "border-color: purple;"),
                                           # select plot type
                                           selectInput("one_d_plot_type", "Select a visualisation:", 
                                                       choices = c(Choose = '', "Adjusted boxplot + kernel density", 
                                                                   "Empirical cumulative distribution function", 
                                                                   "Histogram")),
                                           # add action button
                                           actionButton("runScript_one_d", "Summarise the distribution"),
                                           # Horizontal line
                                           tags$hr(style = "border-color: purple;"),
                                           # add download button
                                           downloadButton("one_d_plot_download", label = "Download plot as pdf file")
                                         )
                                        ),
                                  column(8,
                                         mainPanel(
                                           width = 12,
                                           tableOutput("one_d_summary") %>% withSpinner(color = "#008080"),
                                           tags$br(),
                                           plotOutput("one_d_plot") %>% withSpinner(color = "#008080")
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
                                           uiOutput("select_two_d"),
                                           # Horizontal line
                                           tags$hr(style = "border-color: purple;"),
                                           # select a plot
                                           selectInput("two_d_plot_type", "Select a visualisation:", 
                                                       choices = c(Choose = '', "Adjusted boxplot", 
                                                                   "Empirical cumulative distribution function", 
                                                                   "Kernel density",
                                                                   "Quantile comparison plot")
                                                       ),
                                           # plot the data
                                           actionButton("runScript_two_d", "Compare shot length distributions"),
                                           # Horizontal line
                                           tags$hr(style = "border-color: purple;"),
                                           # add download button
                                           downloadButton("two_d_plot_download", label = "Download plot as pdf file")
                                          )
                                        ),
                                  column(8,
                                         mainPanel(
                                           width = 12,
                                           tableOutput("two_d_summary") %>% withSpinner(color = "#008080"),
                                           tableOutput("two_d_hld") %>% withSpinner(color = "#008080"),
                                           tags$br(),
                                           plotOutput("two_d_plot", width = "100%", height = "670px") %>% withSpinner(color = "#008080")
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
                                           uiOutput("select_multi_d"),
                                           # Horizontal line
                                           tags$hr(style = "border-color: purple;"),
                                           # select a plot
                                           selectInput("multi_d_plot_type", "Select a visualisation:", 
                                                       choices = c(Choose = '', "Adjusted boxplot", 
                                                                   "Empirical cumulative distribution function", 
                                                                   "Kernel density",
                                                                   "Quantile comparison plot")
                                           ),
                                           # plot the data
                                           actionButton("runScript_multi_d", "Compare shot length distributions"),
                                           # Horizontal line
                                           tags$hr(style = "border-color: purple;"),
                                           # add download button
                                           downloadButton("multi_d_plot_download", label = "Download plot as pdf file")
                                         )
                                  ),
                                  column(8,
                                         mainPanel(
                                           width = 12,
                                           tableOutput("multi_d_summary") %>% withSpinner(color = "#008080"),
                                           tableOutput("multi_d_dom") %>% withSpinner(color = "#008080"),
                                           tags$br(),
                                           plotOutput("multi_d_plot", width = "100%", height = "670px") %>% withSpinner(color = "#008080")
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
                                           uiOutput("select_group_one_d"),
                                           textInput("group_one_name", label = "Enter a name for group A", value = "Group A"), 
                                           tags$hr(style = "border-color: #008080"),
                                           uiOutput("select_group_two_d"),
                                           textInput("group_two_name", label = "Enter a name for group B", value = "Group B"),
                                           tags$hr(style = "border-color: purple;"),
                                           # select a plot
                                           selectInput("group_d_plot_type", "Select a visualisation:", 
                                                       choices = c(Choose = '', 
                                                                   "Cliff's d heatmap",
                                                                   "Empirical cumulative distribution function",
                                                                   "HLD heatmap", 
                                                                   "Quantile comparison plot",
                                                                   "Quantile difference plot")
                                           ),
                                           # plot the data
                                           actionButton("runScript_group_d", "Compare shot length distributions"),
                                           # Horizontal line
                                           tags$hr(style = "border-color: purple;"),
                                           # add download buttons
                                           downloadButton("group_d_plot_download", label = "Download plot as pdf file"),
                                           # Horizontal line
                                           tags$hr(style = "border-color: purple;"),
                                           downloadButton("group_d_table_download", label = "Download summary table")
                                         )
                                  ),
                                  column(8,
                                         mainPanel(
                                           width = 12,
                                           tableOutput("group_d_summary") %>% withSpinner(color = "#008080"),
                                           tags$br(),
                                           plotOutput("group_d_plot", width = "100%", height = "670px") %>% withSpinner(color = "#008080")
                                         )
                                  )
                                )
                       )
              )
  )
  
)