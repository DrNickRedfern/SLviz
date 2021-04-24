glossary_panel <- tabPanel(
  "Glossary",
  
  titlePanel("The different summaries and visualisations available in SLviz"),
  p("Click on the name of a summary or visualisation to learn more."),
  tags$hr(style = "border-color: purple;"),
  tags$h4("Numerical summaries"),
  p(tags$a(href = "https://en.wikipedia.org/wiki/Five-number_summary", tags$b("Five number summary: ")), 
    "a set of statistics describing a distribution of shot lengths comprising the minimum shot length, the lower quartile (Q1), the median shot length, the upper quartile (Q3), and the maximum shot length."),
  p(tags$a(href = "https://www.academia.edu/8551326/Comparing_the_Shot_Length_Distributions_of_Motion_Pictures_Using_Dominance_Statistics", tags$b("Hodges-Lehmann difference (HLD):")), 
    "the median value in seconds of the pairwise differences between shot lengths in two films."),
  p(tags$a(href = "https://www.academia.edu/8551326/Comparing_the_Shot_Length_Distributions_of_Motion_Pictures_Using_Dominance_Statistics", tags$b("Cliff's d:")), 
    "a measure of the tendency for shots in one film to have greater duration than those of another film, with range [-1, 1] and stochastic equality between two distributions at d = 0."), 
  p(tags$b("SLviz")," arranges selected films alphabetically when summarising the data for two films or for three to six films. This means that positive values for the Hodges-Lehmann difference and Cliff's d indicate that shots in the first film alphabetically 
    tend the be greater than those in the other film."),
  p("When comparing two groups of films, the pairwise differences are calculated as Group A - Group B and plotted as a", tags$a(href = "https://en.wikipedia.org/wiki/Heat_map", tags$b("heatmap")), " so that positive values indicate that 
    the film from Group A tends to have shots of greater duration than the film from Group B."),
  tags$hr(style = "border-color: purple;"),
  tags$h4("Distributions"),
  p("A ", tags$a(href = "https://www.academia.edu/41900286/Quantile_comparison_of_motion_picture_shot_length_distributions", tags$b("distribution")), "is a representation of the variation of a dataset that enables us to organise and examine data efficiently in order to gain an overall understanding of how the data varies."), 
  p("You can choose from five different visualisations of a shot length distribution:"),
  p(tags$a(href = "https://wis.kuleuven.be/stat/robust/papers/2008/adjboxplot-revision.pdf", tags$b("Adjusted boxplot: ")), "a box plot visualisation of a shot length distribution with an adjustment for skewed distributions."),
  p(tags$a(href = "https://en.wikipedia.org/wiki/Empirical_distribution_function", tags$b("Empirical cumulative distribution function: ")), "the proportion of shots with a duration set less than or equal to some specified value."),
  p(tags$a(href = "https://en.wikipedia.org/wiki/Histogram", tags$b("Histogram: ")), "a non-paramteric estimate of he distribution of shot lengths showing the number of shots in consectuive bins with width = 0.5s. Only available when plotting the shot length data of one film."),
  p(tags$a(href = "https://en.wikipedia.org/wiki/Kernel_density_estimation", tags$b("Kernel density: ")), "a non-parametric estimate of the distribution of shot lengths showing the density of shot lengths."),
  p(tags$a(href = "https://www.academia.edu/41900286/Quantile_comparison_of_motion_picture_shot_length_distributions", tags$b("Quantiles: ")), "quantile comparison plots visualise the quantiles of a shot length distribution from 0.05 to 0.95, with steps of 0.05.
    When comparing two films, a plot showing the difference between the quantiles of each film is also plotted. For two groups of films, a plot showing the distribution of the pairwise differences for every quantile between the two groups 
    may be selected, where the difference between quantiles is calculated as Group A - Group B, so that positive differences indicate the quantiles in Group A are larger."),
  tags$hr(style = "border-color: purple;"),
  tags$h4("Time series"),
  p(tags$a(href = "https://en.wikipedia.org/wiki/Counting_process", tags$b("Counting process: ")), 
    "the number of shots in a film (", tags$em("N"), ") that have occurred up to and including time ", tags$em("t"), "."),
  p(tags$a(href = "https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.458.6446&rep=rep1&type=pdf", tags$b("Cut density: ")), 
    "fits a kernel density estimate to the set of cut times for a film."),
  p(tags$a(href = "https://computationalfilmanalysis.wordpress.com/2020/07/16/loessggplot/", tags$b("Multiple loess smoothers: ")), 
    "fit a set of loess smoothers to the shot length data of a film, iterating over spans in the range [0.1, 0.9] with steps of 0.01. This option is only available when plotting the time series for one film."),
  tags$br()
)