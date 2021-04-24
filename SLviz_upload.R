# upload data into the app

upload_sidebar <- sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose a CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/tab-separated-values",
                           "text/comma-separated-values,text/plain",
                           ".txt",
                           ".csv",
                           ".tsv")),
      
      # Horizontal line ----
      tags$hr(style = "border-color: purple;"),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ",")
)

upload_main <- mainPanel(textOutput("f_name"), 
                             tableOutput("file"), 
                             textOutput("str_t"), tags$br(),
                             verbatimTextOutput("structure"))

upload_panel <- tabPanel(
  "Upload data",
  
  titlePanel("Upload your data to SLviz"),
 
  p("Shot length data should be in a", tags$b("csv"), " file, with one column of shot length data per film (i.e. in wide format).", tags$b("SLviz"), "will remove NAs during data processing."),
  p(tags$b("SLviz"), " will use the name of each column in your csv file to select data and to create legends and titles for the different visualisations. 
    Naming the columns in your dataset is therefore strongly advised. 
    If your csv file does not have column headers, be sure to untick the", tags$b("header"), "checkbox below."),
  p("Please make sure you have selected the correct type of separator for your csv file.
    If the structure of your data is reported incorrectly, try selecting a different separator."),
  
  sidebarLayout(upload_sidebar, upload_main)
)
