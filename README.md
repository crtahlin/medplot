# medplot R package

Functions for drawing graphs in R visualizing medical information.

You can find the Excel template file (to use with the medplot shiny app) in the folder:

/inst/extdata/PlotSymptoms_shiny.xlsx

And the Tab Separated Values template file in the folder:

/inst/extdata/PlotSymptoms_TemplateTSV.txt

# Installation instructions for the medplot package

After installing the [R programming language](http://cran.r-project.org/), use the console in R to install the devtools package:

```{r, eval=FALSE}
install.packages("devtools")
```

Make sure you install the development tools mentioned on the [devtools web page](http://www.rstudio.com/products/rpackages/devtools/). For MS Windows, this would be the [Rtools package](http://cran.r-project.org/bin/windows/Rtools/). 

Perl has to be installed in order to enable importing of MS Excel files. For MS Windows, [Strawberry perl](http://www.perl.org/get.html) should work. Reboot computer.

Load devtools and install packages from GitHub:

```{r, eval=FALSE}
library(devtools)
install_github("crtahlin/medplot")
```

# Running medplot localy

To run medplot:
```{r, eval=FALSE}
library(medplot)
medplotOnline()
```

This should open the application in a web browser.

# Troubleshooting Linux installation

Some specific problems arose while installing on Linux (Ubuntu 14.04). Solutions are listed below.

Prerequisite for the devtools package - use terminal to:

     sudo apt-get install libcurl4-openssl-dev


Prerequisite for the Cairo & medplot packages - use terminal to:

     sudo apt-get install libcairo2-dev
     sudo apt-get install libxt-dev     
as described in http://stackoverflow.com/questions/16678934/r-cairo-installation-without-apt-get-sudo

Prerequisite for the XML & medplot package - use terminal to:

     sudo apt-get install r-cran-xml
as described in http://stackoverflow.com/questions/7765429/unable-to-install-r-package-in-ubuntu-11-04


<!--
Notes on installation in Linux

To install R
sudo apt-get install r-base-core 

To get devtools working
sudo apt-get install libcurl4-openssl-dev

To install devtools
install.packages("devtools")

To install Cairo library
sudo apt-get install libcairo2-dev
sudo apt-get install libxt-dev
as described in http://stackoverflow.com/questions/16678934/r-cairo-installation-without-apt-get-sudo

To install XML package
sudo apt-get install r-cran-xml
as described in http://stackoverflow.com/questions/7765429/unable-to-install-r-package-in-ubuntu-11-04

To install medplot
library("devtools")

To install shinyIncubator
install_github(username="rstudio", repo="shiny-incubator")

To run medplot:
medplotOnline()
-->