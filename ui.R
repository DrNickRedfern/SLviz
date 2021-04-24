source("SLviz_intro.R")
source("SLviz_glossary.R")
source("SLviz_upload.R")
source("SLViz_distributions.R")
source("SLviz_ts.R")
source("SLviz_about.R")

ui <- navbarPage(
  "SLviz",
  
  theme = shinytheme("darkly"), 
  
  intro_panel,
  glossary_panel,
  upload_panel,
  distributions,
  time_series,
  about_panel,
  
  tags$head(
    tags$style(HTML(".navbar-default {background-color: #440154 !important;}")))
  
)