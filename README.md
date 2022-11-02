# SLviz
As a motion picture typically comprises several hundred (if not thousands) of shots, discussion of its editing style can be challenging. Visualising the editing of a film is the best way of making the wealth of editing information available in a film manageable.

The use of data visualisation should be common practice in the study of film style as means of simplifying the description of editing data, allowing us to to look deeper into that data, and to share the results.

**SLviz** is a Shiny app for visualising motion picture shot length data implemented using the R statistical programming language that lets you choose from a range of visualisations in order to describe and analyse your shot length data, and to communicate your results.

![SLviz_demo](/images/SLviz_demo.png)

## Set up and use the app

### Download SLviz as an executable (Windows only)
SLviz is now available as an executable file for Windows. Download the exe file and install in your desired directory (NB: requires administrator privileges to install). Please this will also install R 4.2.1 if required along with the packages required for R and the app to run.
Open the releases page of the app’s GitHub repository and click the `setup_SLviz.exe` to download the file.

### Runt he app from GitHub
It will be necessary to run some code to set up **SLviz**, but you can just copy the code in the instructions below and paste it into the console in RStudio and hit `Enter`. Once the app is running, no further coding is required.

Before using **SLviz** for the first time you will need to:

* Download and install R
* Download and install RStudio
* Install the required packages

All of the software required to run **SLviz** is freely available.

#### Step 1: install R
To download R, go to the [Comprehensive R Archive Network (CRAN)](https://cran.r-project.org), and download and install the latest version of R appropriate to your system.

#### Step 2: install RStudio
RStudio is an integrated development environment (IDE) for R. Install the latest release of RStudio for your system available from [https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/).
 
Once installed, open RStudio to complete the next step.

#### Step 3: install the required packages
Packages extend the functionality of R, and in order to run **SLviz** you will need to install a set of packages in order for the app to work. Paste the code below into the console in RStudio and hit `Enter`.

```R
install.packages(c("shiny", "shinycssloaders", "shinythemes", "robustbase", "tidyverse", "viridis", "ggpubr", "arrangements", "ggtext"))
```

You only need to complete steps 1 through 3 the first time you run **SLviz**.

#### Step 4: start the app
You can run **SLviz** by opening RStudio and pasting the following code into the console in and hitting `Enter`.

```R
shiny::runGitHub("SLviz", "DrNickRedfern", ref = "main")
```

## Running SLviz

### Upload some data
**SLviz** needs data and so the first step is to upload a *csv* file containing the shot length data for one or more films. **SLviz** will accept comma, tab, or semi-colon separated csv files. Uploading other file types (such as Excel spreadsheets with xls or xlsx extensions) to **SLviz** will return an error.

The data should be in *wide format*, with one column of shot length data per film. **SLviz** will remove `NAs` during data processing.

**SLviz** will use the name of each column in your csv file to select data and to create legends and titles for the different visualisations. Naming the columns in your dataset is therefore *strongly* advised. If your csv file does not have column headers, be sure to untick the *header* checkbox before visualising your data.

**SLviz** will return a summary of the structure of your uploaded csv file. Please make sure you have selected the correct type of separator for your csv file. If the structure of your data is reported incorrectly, try selecting a different separator.

### Check the glossary
**SLviz** has a glossary containing brief descriptions of the different summaries and visualisations available. Click on the name of a summary or visualisation to learn more.

### Visualisations
You can visualise either the distribution of shot lengths or the time series of editing in a single film , in two films , or in three to six films. To visualise more than six films it is advisable to break the overall number of films into smaller groups and produce a plot for each group. This will ensure the plots will retain a good level of clarity.

Two groups of films can also be visualised by selecting different groups of films from a single csv file. Results will be bsaed on groups rather than on individual films.

The visualisations you can select from include adjusted boxplots, kernel densities, histograms, empirical cumulative distribution functions, quantile comparison plots, heatmaps, multiple loess smoothers, and counting process plots.

### Summaries
In addition to visualising your shot length data, **SLviz** will also return the five-number summary for each film. For pairwise comparisons of shot length data, [Cliff's *d*](https://www.academia.edu/8551326/Comparing_the_Shot_Length_Distributions_of_Motion_Pictures_Using_Dominance_Statistics) statistic is returned as a measure of the extent to which shots in one film tend to have longer duration than shots in another film, along with the [Hodges-Lehmann difference](https://www.academia.edu/8551326/Comparing_the_Shot_Length_Distributions_of_Motion_Pictures_Using_Dominance_Statistics).

### Downloading plots
To download a plot of your data, click on the *Download plot as pdf file* button. 

All plots will have a width of 15.92 centimetres so that they will fit onto an A4 page with margins of 2.54 cm (or one inch). Depending on the visualisation selected, the height of a plot varies between 10.2 and 21.0 centimetres. Again, this is so they will fit onto an A4 page assuming 2.54 cm (one inch) margins and leave enough room for a figure caption. Plots are downloaded as pdf files with a resolution of 600 dpi.

All of the plots in **SLviz** use the viridis colour palette and so will convert nicely to grayscale if required for publication.

### Citing **SLviz**
If you use **SLviz** in your research please use the following citation:

  * Redfern, N. (2021) SLviz (Version v0.3.0). Zenodo. [http://doi.org/10.5281/zenodo.4720009](http://doi.org/10.5281/zenodo.4720009)
