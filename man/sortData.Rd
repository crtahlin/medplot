\name{sortData}
\alias{sortData}
\title{Sort results data}
\usage{
sortData(data, sortMethod = "BEA", nUNITS, DATES.COLUMN.FIRST,
  DATES.COLUMN.LAST, TEST.RESULT.LEVELS)
}
\description{
Function that sorts the data according to a criterion. Not
to be called directly by the user.
}
\details{
Function is called by the plotTests function, to sort the
data before it starts working on it. Different methods of
sorting can be used. "DateIn" sorts by the date of
admission. "BEA" uses seriation package and Bond Energy
Algorithm. "BEA_TSP" as above with TSP to optimize the
measure of effectivenes. "PCA" uses seriation package First
principal component algorithm.
}

