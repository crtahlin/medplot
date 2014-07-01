#define a ggplot2 theme
myTheme <- function () {
  myTheme <- theme_bw() + theme(
    text = element_text(size=18)
  )
  return(myTheme)}