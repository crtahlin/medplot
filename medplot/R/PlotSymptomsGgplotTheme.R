#' @title Define a ggplot2 theme used in app
#' 
#' @export
myTheme <- function () {
  myTheme <- theme_bw() + theme(
    text = element_text(size=18)
    )
  return(myTheme)
  }