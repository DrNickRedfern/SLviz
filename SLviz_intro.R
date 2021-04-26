intro_panel <- tabPanel(
  "Welcome",
  
  titlePanel("Visualising motion picture shot length data"),
  
  tags$blockquote(tags$i("The best single device for suggesting, and at times answering, questions beyond those originally posed is the graphical display."),
                  tags$br(), tags$br(), "John Tukey", cite = "John Tukey"),
  
  p("Good data visualisations are ", tags$em("descriptive"), 
    ", summarising the information in a data set simply and efficiently; they are ",
    tags$em("analytical"), ", allowing you to discover what is important and interesting about your data; and they are ", 
    tags$em("communicative"), ", allowing you to share your insights directly with your audience."),
 p("As a motion picture typically comprises several hundred (if not thousands) of shots, discussion
    of its editing style can be challenging. Visualising the editing of a film is the best way of making the wealth of editing information available in a film manageable."),
  p("The use of data visualisation should be common practice in the study of film style as means of simplifying the description of editing data, allowing us to to look deeper into that data, and to share the results."),
  p(tags$b("SLviz"), " lets you choose from a range of visualisations in order to describe and analyse your shot length data, 
  and to communicate your results. The outputs can be downloaded as a ", tags$b("pdf"), " file for inclusion in an assignment or journal article."),
  
  tags$hr(style = "border-color: purple;"),
  tags$h4("Using ", tags$b("SLviz")),
  p("To get started you need to upload some shot length data to ", tags$b("SLviz"), ". You can do this on the ", tags$b("Upload data"),
    "page. Once uploaded, you will be able to choose the data for a particular film(s) on the other pages in the app."),
  p("You can visualise either the ", tags$b("distribution"), " of shot lengths or the", tags$b("time series"), " of editing in a ", 
    tags$b("single film"), ", in ", tags$b("two films"), ", or in ", tags$b("three to six"), "films. 
    To visualise more than six films it is advisable to break the overall number of films into smaller groups and produce a plot for each group. 
    This will ensure the plots will retain a good level of clarity."),
  p(tags$b("Two groups"), "of films can also be visualised by selecting different groups of films from a single csv file. Results will be based on groups rather than on individual films."),
  p("The ", tags$b("glossary"), "contains brief descriptions of each of the summaries and visualisations available in ", tags$b("SLviz"), "."),
  
  tags$hr(style = "border-color: purple;"),
  tags$h4("Citing ", tags$b("SLviz")),
  p("If you find", tags$b("SLviz")," useful as part of your research, please cite the app using the following reference:"),
  p("Redfern, N. (2021) SLviz (Version v0.1.0). Zenodo. http://doi.org/10.5281/zenodo.4720009.", tags$br(), 
          tags$a(href = "http://doi.org/10.5281/zenodo.4720009", 
          img(src="https://zenodo.org/badge/DOI/10.5281/zenodo.4720009.svg"))),
  tags$br()
)
