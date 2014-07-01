\name{plotPropPositive}
\alias{plotPropPositive}
\title{Plot proportions with symptoms}
\usage{
plotPropPositive(data, grouping = "Sex", measurements = "Measurement",
  symptomsNames)
}
\arguments{
  \item{data}{Data to be passed to the function as a data
  frame.}

  \item{grouping}{The column name of the binary grouping
  variable used to define the groups on the plot.}

  \item{measurements}{The column name of the variable
  containing measurement occasions.}

  \item{symptomsNames}{A vector of column names containing
  the symptom severity.}
}
\description{
Function that plots proportion of subjects that have a
certain symptom present.
}

