\name{plotTests}
\alias{plotTests}
\title{Plot test results}
\usage{
  plotTests(data, figureParameters, fileName,
    generateTooltips = TRUE, sortMethod = "DateIn")
}
\description{
  Function that plots which test results were positive for
  a subject on a certain day.
}
\details{
  The function is primarily meant to be called from MS
  Excel through the RExcel plugin and not used directly
  from R. Data is sent from Excel and drawn on a graph by
  this R function. The output contains two graphs: A and B.

  The A graph shows the test results for subjects (y-axis)
  on certain days (x-axis). The results are represented by
  colored dots and can be divided in groups and subgroups
  (two levels), depending on the nature of data.

  The B graph shows, via a barchart, how many results were
  positive on a certain day for a certain group.
}

