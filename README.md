# SLviz
A shiny app for visualising motion picture shot length data

It will be necessary to run some code to set up **SLviz**, but you can just copy the code in the instructions below and paste it into the console in RStudio and hit `Enter`. Once the app is running, no further coding is required.

## Installation and setup
**SLviz** is a Shiny app - that is, it uses the Shiny application to provide an interactive way of using the R statistical programming language.

Before using **SLviz** for the first time you will need to:

* Download and install R
* Download and install RStudio
* Install the required packages

All of the software required to run **SLviz** is freely available.

### Step 1: install R
To download R, go to the [Comprehensive R Archive Network (CRAN)](https://cran.r-project.org), and download andinstall the latest version of R appropriate to your system.

### Step 2: install RStudio
RStudio is an integrated development environment (IDE) for R. Install the latest release of RStudio for your system available from [https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/).
 
Once installed, open RStudio to complete the next step.

### Step 3: install the required packages
Packages extend the functionality of R, and in order to run **SLviz** you will need to install a set of packages in order for the app to work. Paste the code below into the console in RStudio and hit `Enter`.

```R
install.packages(c("shiny", "shinycssloaders", "shinythemes", "robustbase", "orddom", "tidyverse", "viridis", "ggpubr", "arrangements", "DescTools"))
```

## Running SLviz

### Start the app
You can run **SLviz** by opening RStudio and pasting the following code into the console in and hitting `Enter`.

```R
shiny::runGitHub("SLviz", "DrNickRedfern", ref = "main")
```

### Upload some data
**SLviz** needs data and so the first step is to upload a *csv* file containing the shot length data for one or more films. **SLviz** will accept comma, tab, or semi-colon separated csv files. Uploading other file types (such as Excel spreadsheets with xls or xlsx extensions) to **SLviz** will return an error.

The data should be in *wide format*, with one column of shot length data per film. **SLviz** will remove `NAs` during data processing.

**SLviz** will use the name of each column in your csv file to select data and to create legends and titles for the different visualisations. Naming the columns in your dataset is therefore *strongly* advised. If your csv file does not have column headers, be sure to untick the *header* checkbox before visualising your data.

**SLviz** will return a summary of the structure of your uploaded csv file. Please make sure you have selected the correct type of separator for your csv file. If the structure of your data is reported incorrectly, try selecting a different separator.

### Check the glossary
**SLviz** has a glossary containing brief descriptions of the different summaries and visualisations available. Click on the name of a summary or visualisation to learn more.

### Visualisations

### Downloading plots
To download a plot of your data, click on the *Download plot as pdf file* button. 

Plots are downloaded as pdf files with a resolution of 600 dpi. 

All plots will have a width of 15.92 centimeters so that they will fit onto an A4 page with margins of 2.54 cm (or one inch).

Depending on the visualisation selected, the height of a plot varies between 10.2 and 21.0 centimeters. Again, this is so they will fit onto an A4 page assuming 2.54 cm (one inch) margins and leave enough room for a figure caption.

All of the plots in **SLviz** use the viridis colour palette and so will convert nicely to grayscale if required for publication.

### Citing **SLviz**
If you use **SLviz** in your research please use the following citation:

  * Redfern, N. (2021) SLviz (version 0.1.0).
