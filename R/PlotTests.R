#' @title Plot test results
#' @description Function that plots which test results were positive for 
#' a subject on a certain day.
#' @details
#' The function is primarily meant to be called from MS Excel through the 
#' RExcel plugin and not used directly from R.
#' Data is sent from Excel and drawn on a graph by 
#' this R function. The output contains two graphs: A and B. 
#' 
#' The A graph shows the test results for subjects (y-axis) on certain days
#' (x-axis). The results are represented by colored dots and can be divided in 
#' groups and subgroups (two levels), depending on the nature of data.
#' 
#' The B graph shows, via a barchart, how many results were positive on
#' a certain day for a certain group.
#' 
plotTests <- function (data, figureParameters, fileName,
                       generateTooltips = TRUE, sortMethod="DateIn") {
  
  # replace missing values with NA (must do because RExcel sends them as 
  # empty string and it does that by purpose - at a different setting 
  # the date labels would also be transformed, which we do not want
  data[data==""] <- NA
  # drop unused levels in data frame factors
  data <- droplevels(data)
  
  # start profiling
  # Rprof(filename="Rprof.out", append=TRUE)
  
  # initialize placeholder for error messages 
  errorMessages <<- list()
  criticalError <- FALSE
  
  # check if function arguments were received; a bit superflous since
  # function will not work without arguments passed
  if(!exists(x="data")) {
    errorMessages[length(errorMessages)+1] <<-
      "Error: Data was not passed to R."
  }
  
  if(!exists(x="figureParameters")) {
    errorMessages[length(errorMessages)+1] <<-
      "Error: Plot parameters were not passed to R."
  }
  
  
  # identify and store first and last columns that contain dates of tests
  datesColumnsIndices <- grep("^X[0-9]{1,2}[.][0-9]{1,2}[.][0-9]{4}$",
                              colnames(data))
  DATES.COLUMN.FIRST <- min(datesColumnsIndices)
  DATES.COLUMN.LAST <- max(datesColumnsIndices)
  # number of date columns
  nDATES <- DATES.COLUMN.LAST - DATES.COLUMN.FIRST + 1
  
  # identify and store parameters for drawing (levels of test results, color,
  # size of symbols) 
  TEST.RESULT.LEVELS <- as.character(unlist(figureParameters["Result"]))
  DOT.COLORS <- as.character(unlist(figureParameters["Color"]))
  DOT.SIZES <- as.integer(unlist(figureParameters["Size"]))
  
  # make dot colors transparent with transparency APLHA (0-255)
  ALPHA <- 150
  DOT.COLORS <- apply(col2rgb(DOT.COLORS), 2, function(x)
  {rgb(red=x[1], green=x[2], blue=x[3], alpha=ALPHA, maxColorValue=255)} )
  
  # set the color for the lines at unit level
  LINE.UNIT.COLOR <- rgb(red=col2rgb("gray")[1], 
                         green=col2rgb("gray")[2],
                         blue=col2rgb("gray")[3],
                         alpha=ALPHA,
                         maxColorValue=255)
  
  # count the number of units in sample
  nUNITS <- dim(data)[1]
  
  # store the dates in R date format
  datesofTests <- as.Date(colnames(data)[datesColumnsIndices], "X%d.%m.%Y")
  
  # vector of relative difference in days from test on day x to test on day 1
  daysofTests <- datesofTests-datesofTests[1]
  
  # create table of frequencies for all USED combinations of types & diagnosis
  tableofGroups <- table(data$Type, data$Diagnosis, useNA="ifany")
  
  # walk through all the units in the sample and assign them 
  # an absolute line number (keeping spaces for separating lines between groups)
  nTYPES <- dim(tableofGroups)[1]
  nDIAGNOSIS <- dim(tableofGroups)[2]
  TYPE.LEVELS <- dimnames(tableofGroups)[[1]]
  DIAGNOSIS.LEVELS <- dimnames(tableofGroups)[[2]]
  # set size of space for separating lines on graph
  LINE.SIZE <- 4
  
  # Graph B settings 
  # set buffer between bars plotted in relative units of one bar width
  # e.g. if 1, that means buffer is the same width as one bar
  BAR.BUFFER <- 3
  
  # reorder data (using seriation package) unless no sorting requested
  if (sortMethod!="none") {
    data <- sortData(data, sortMethod, 
                     nUNITS, 
                     DATES.COLUMN.FIRST,DATES.COLUMN.LAST,
                     TEST.RESULT.LEVELS )
  }
  
  # add an ordering column to the data dataframe; the number means which row
  # the unit should be drawn in
  order <- vector(mode="integer", length=nUNITS)
  data <- cbind(data, Order=order) 
  rm(order)
  
  # start with line number 1; number will be incremented for every unit and every
  # separating line
  lineNo <- 1
  
  # assign a rownumber into column "Order" for each unit
  for (i in 1:nTYPES) {
    for (j in 1:nDIAGNOSIS) {
      # lookup cell size and go to next cell in table if no units are found
      cellSize <- tableofGroups[i,j]
      if (cellSize == 0) {next}
      
      # find names for cell dimension
      typeName <- dimnames(tableofGroups)[[1]][i]
      diagnosisName <- dimnames(tableofGroups)[[2]][j]
      
      # catch a special case, when 1st level is NA
      if (is.na(typeName)) {
        # a special case when both 1st and 2nd level are NA
        if (is.na(diagnosisName)) {
          data[which(is.na(data$Type) & is.na(data$Diagnosis)),]["Order"] <- 
            (lineNo):(lineNo-1 + cellSize)
          # increase line number by cellsize and buffer for the separating line
          lineNo <- lineNo + cellSize + LINE.SIZE
          
          # case when only 1st level is NA
        } else {
          # assign an abolute order number to units in this cell
          data[which(is.na(data$Type) & data$Diagnosis==diagnosisName),]["Order"] <-
            (lineNo):(lineNo-1 + cellSize)
          # increase line number by cellsize and buffer for the separating line
          lineNo <- lineNo + cellSize + LINE.SIZE
        }
      } else {
        # catch a special case when only 2nd level is NA
        if (is.na(diagnosisName)) {
          data[which(data$Type==typeName & is.na(data$Diagnosis)),]["Order"] <- 
            (lineNo):(lineNo-1 + cellSize)
          # increase line number by cellsize and buffer for the separating line
          lineNo <- lineNo + cellSize + LINE.SIZE
        } else {
          # assign an abolute order number to units in this cell
          data[which(data$Type==typeName & data$Diagnosis==diagnosisName),]["Order"] <- 
            (lineNo):(lineNo-1 + cellSize)
          # increase line number by cellsize and buffer for the separating line
          lineNo <- lineNo + cellSize + LINE.SIZE
        }
      }
    }
  }
  
  # load plotting library
  library(Cairo)
  
  # set width of plotting region (in centimeters)
  PLOT.WIDTH <- max(c(max(daysofTests), (5+3*length(TEST.RESULT.LEVELS)) ))
  
  Cairo(fileName,
        type="svg",
        width=PLOT.WIDTH ,
        height=29,
        units="cm",
        dpi=300,
        pointsize=3)
  
  # Cairo("example.pdf", type="pdf",width=19,height=24,units="cm",dpi=300)
  # svg("example1.svg")
  
  #set layout
  layout(c(1,2), heights=c(4,1))
  
  # set margins for graph A
  par(mar=c(5, 7, 4, 2) + 0.1 )
  
  
  # generate plot with number of days of tests on the x axis
  # and appropriate y axis
  plot(TRUE,
       # plot y axis with buffer for lines between groups
       # the top of the plot starts at 0 and goes down 
       # to the total of lines drawn
       ylim=c(lineNo, 0),
       # plot x axis with one column for every date
       xlim=c(0, max(daysofTests)),
       # plot nothing - just prepare the "canvas"
       type="n", axes=FALSE, xlab="Date", ylab=""
  )  
  
  # initialize counter of how many symbols are drawn
  pointCounter <- 0
  # initialize list that will hold additional info for symbol tooltips
  symbolTooltip <- list()
  # draw symbols of test results for each unit via their ordering
  for (i in 1:lineNo) {
    # skip drawing if there is no unit in this line number
    if (!any(data$Order==i)) 
      next
    # otherwise draw each point of the line, 
    # but only the columns that are not NA
    # calculate the indices of nonNA test values
    index <- which(!is.na(data[data$Order==i, DATES.COLUMN.FIRST : DATES.COLUMN.LAST]))
    
    for (ii in index) {
      # get a character vector of all the tests in the cell
      tests <- isolateTests(string=as.character(unlist(
        data[data$Order==i, DATES.COLUMN.FIRST : DATES.COLUMN.LAST][ii]
      )), separator=",")
      # TODO: would the code run faster, if I would use as reference:
      # data[DATES.COLUMN.FIRST + ii - 1])
      # as bellow ?
      
      # test if all test results entered are valid
      for (iii in 1:length(tests)) {
        if (!any(tests[iii]==TEST.RESULT.LEVELS)){
          errorMessages[length(errorMessages)+1] <<-
            paste("Error: Data does not match allowed values. Check ID:",
                  data[data$Order==i,]["ID"],
                  "and date column:",
                  names(data[DATES.COLUMN.FIRST + ii - 1]),
                  "for value:'",
                  tests[iii],
                  "'"
            )
          # if at least one is invalid, set that a critical error has occured
          # as TRUE     
          criticalError <- TRUE
        }
      }
      
      # draws point one on top of the other if multiple tests 
      # are positive in the same cell
      points(
        # draw x coordinate for this cell  
        # repeat as many times as there are test results in the cell
        x=rep(daysofTests[ii], length(tests)),
        # draw the line number as the y coordinate, as many times
        # as there are test results in the cell
        y=rep(i, length(tests)),
        # use the color that is defined for each test
        # by comparing the values in the cell with colors defined as parameters
        col=DOT.COLORS[unlist(lapply(tests,
                                     function(x) which(x==TEST.RESULT.LEVELS) ) )],
        # use the size of dot that is defined for each test
        cex=DOT.SIZES[unlist(lapply(tests,
                                    function(x) which(x==TEST.RESULT.LEVELS) ) )],
        pch=16
      )
      
      # save some additional information for each point 
      # to be shown as a tooltip
      # loop through all points - for case when there are two in one cell
      for (k in 1:length(tests)) {
        symbolTooltip[pointCounter+k] <-
          paste("ID:", data[data$Order==i, "ID"], # "ID" of patient     
                "; result:", # lists test results
                as.character(unlist(data[data$Order==i,
                                         DATES.COLUMN.FIRST : 
                                           DATES.COLUMN.LAST][ii])),
                "; date:", format(datesofTests[ii],format="%d.%m.%y" )# Date of test
          )
      }
      
      # increment counter of symbols drawn
      pointCounter <- pointCounter + length(tests)
    }
    
    # draw death of patient if they died, at the date of release
    # but only compare non-NA values of Outcome
    if (!is.na(data[data$Order==i,]$Outcome)) {
      if (data[data$Order==i,]$Outcome=="died") {
        dateofDeath <- as.Date((data[data$Order==i,]$DateOut))
        dayofDeath <- dateofDeath - datesofTests[1]
        if (dayofDeath < 0) {
          errorMessages[length(errorMessages)+1] <<-
            paste("Error: Day of death falls before the date of first test.")
          criticalError <- TRUE
        }
        if (dayofDeath > max(daysofTests)) {
          errorMessages[length(errorMessages)+1] <<-
            paste("Warning: Day of death falls after the last date of
                  observation.")
        }
        if (dayofDeath >= 0 && dayofDeath <= max(daysofTests)) {
          plotDeaths(lineNumber=i, dayofDeath= dayofDeath + 1)
          symbolTooltip[pointCounter+1] <-
            paste("ID:", data[data$Order==i, "ID"], # "ID" of patient 
                  "; died:", format(dateofDeath, format="%d.%m.%y"))
          pointCounter <- pointCounter + 1 
        }
      }
    }
  }
  
  
  # draw labels on the x axis
  axis(1, at=daysofTests, labels=rep("", length(daysofTests)), cex.axis=.35)
  DATE.LABELS <- format(datesofTests, format="%d %b")
  axis(1, at=daysofTests[seq(1, length(daysofTests), 2)], 
       labels=DATE.LABELS[seq(1, length(daysofTests), 2)], 
       cex.axis=.75, line=-0.5, lwd=0)
  axis(1, at=daysofTests[seq(2, length(daysofTests), 2)], 
       labels=DATE.LABELS[seq(2, length(daysofTests), 2)],
       cex.axis=.75, line=-1, lwd=0)
  
  # draw horizontal lines for each unit
  abline(h=unlist(data["Order"]), lty=2, col=LINE.UNIT.COLOR)
  
  # call function to draw labels for each existing 1st+2nd level combination 
  for (i in TYPE.LEVELS) {
    for (ii in DIAGNOSIS.LEVELS) {
      if (checkifCombinationExists(data, i, ii)) { 
        drawLevelLabels(data, TYPE.LEVELS, LINE.SIZE, DIAGNOSIS.LEVELS,
                        firstLevel=i, secondLevel=ii)
      }
    }
  }
  
  # add label of graph A
  mtext("A", side=3, adj=0, line=1, cex=1.5)
  
  # add a legend to the A plot
  legend("topleft",
         # x=2,
         # y=-3,
         bty="n",
         xpd=TRUE,
         legend=TEST.RESULT.LEVELS,
         pch=16,
         horiz=TRUE,
         col=DOT.COLORS,
         pt.cex=DOT.SIZES
  )
  
  # if critical error occured, print a warning on graph
  if (criticalError) {
    text(x=1, y=0, labels="Critical error",
         cex=max(c(8, PLOT.WIDTH/15)), col="red",
         srt=-45, adj=0)
  }
  
  
  #### draw graph B #####

  # set margins for 2nd figure
  par(mar=c(4, 7, .5, 2) + 0.1 )
  
  # create data structure to hold data for graph of percentage of positive units
  positiveUnits <- matrix(ncol=length(DATES.COLUMN.FIRST : DATES.COLUMN.LAST),
                          nrow=length(TYPE.LEVELS))
  
  # calculate percentages of positive (nonNEGATIVE) units in all results
  # per 1st level group for each of the dates
  column <- 1
  for (i in DATES.COLUMN.FIRST : DATES.COLUMN.LAST) { # cycle through all dates (columns)
    row <- 1
    for (ii in TYPE.LEVELS) { # cycle through all 1st level levels (rows)
      # for case when 1st level in nonNA
      if (!is.na(ii)) { 
        sumNonNA <- sum(!is.na(data[data$Type==ii, i]))
        sumPositive <- sum(!is.na(data[data$Type==ii, i]) &
                             data[data$Type==ii, i]!="neg" ) 
        positiveUnits[row, column] <- sumPositive/sumNonNA*100
      }
      # for case when 1st level is NA
      if (is.na(ii)) {
        sumNonNA <- sum(!is.na(data[is.na(data$Type), i]))
        sumPositive <- sum(!is.na(data[is.na(data$Type), i]) &
                             data[is.na(data$Type), i]!="neg" ) 
        positiveUnits[row, column] <- sumPositive/sumNonNA*100
      }
      row <- row + 1
    }
    column <- column +1 
  }
  
  
  
  # calculate minumum difference between consecutive tests in days
  minimumInterval <- min(as.numeric(diff(daysofTests)))
  # calculate space available to draw a bar, 
  # leaving a buffer the size of one width
  barWidth <- minimumInterval / (length(TYPE.LEVELS)+BAR.BUFFER)
  # calculate distance to middle of bars drawn
  middleofBars <- (barWidth*(length(TYPE.LEVELS))/2)
  
  # initialize spacing vector (vector of spaces between bars)
  spacingVector <- vector(mode="numeric")
  # generate spacing vector for drawing bars appropriately spaced
  # each number is the space before each column
  # go through all columns
  for (i in 1:(dim(positiveUnits)[1]*dim(positiveUnits)[2])) {
    if (i == 1) { # case of first column - align the middle of all columns above date
      spacingVector[i] <- (-middleofBars/barWidth)
    } else { 
      if (i %% length(TYPE.LEVELS) == 1) { 
        # for every column that belongs to a new set of columns
        # calculate how far appart tests are in days 
        # and adjust shift for this difference
        spacingVector[i] <- (daysofTests[(i %/% length(TYPE.LEVELS))+1] -
                               daysofTests[(i %/% length(TYPE.LEVELS))]
                             - 2*middleofBars) /(barWidth)
      } else if (length(TYPE.LEVELS)==1) { # if only one column (level) in group
        spacingVector[i] <- (diff(daysofTests)[i-1] - 2*middleofBars)/(barWidth)
      }
      else { # all columns inside a group have zero space between
        spacingVector[i] <- 0
      }
    }
  }
  
  # load libray for "barplot2" function
  library(gplots)
  # load library for "ColorBrewer" function
  library(RColorBrewer)
  # only use ColorBrewer if there are more than two levels
  # and less than 10 levels which is too much for the "Pastel1" pallette
  if (length(TYPE.LEVELS)>2 & length(TYPE.LEVELS)<10) {
    legendColors <- brewer.pal(n=length(TYPE.LEVELS), name="Pastel1")
  }
  # if one level use black colors for columns
  if (length(TYPE.LEVELS)==1) {legendColors <- c("black")}
  # if two levels use black and white colors for columns
  if (length(TYPE.LEVELS)==2) {legendColors <- c("black", "white")}
  # if more than 9 levels just take random colors out of pallette
  # (more than 9 levels are not expected)
  if (length(TYPE.LEVELS)>10) {
    legendColors <- sample(colors(),length(TYPE.LEVELS), replace=FALSE)
  }
  
  barplot2(positiveUnits, beside=T, col=legendColors, axes=F,
           xlim=c(0, max(daysofTests)), width=barWidth,
           space=spacingVector)
  abline(h=seq(0, 100, by=5), col="gray", lty=2)
  # draw labels on the x axis
  axis(1, at=daysofTests, labels=rep("", length(daysofTests)), cex.axis=.35)
  
  # draw x axis labels
  DATE.LABELS <- format(datesofTests, format="%d %b")
  axis(1, at=daysofTests[seq(1, length(daysofTests), 2)], 
       labels=DATE.LABELS[seq(1, length(daysofTests), 2)], 
       cex.axis=.75, line=-0.5, lwd=0)
  axis(1, at=daysofTests[seq(2, length(daysofTests), 2)], 
       labels=DATE.LABELS[seq(2, length(daysofTests), 2)],
       cex.axis=.75, line=-1, lwd=0)
  
  # draw tick marks on the y axis
  axis(2, cex.axis=.5)
  
  # draw title of y axis
  mtext("Percentage of positive swabs", line=2, side=2, cex=.7 )
  
  # draw legend for graph B
  TEMP.TYPE.LEVELS <- TYPE.LEVELS
  TEMP.TYPE.LEVELS[(is.na(TYPE.LEVELS))]<-"Unknown"
  legend("topright",  legend=TEMP.TYPE.LEVELS, fill=legendColors, cex=.7)
  rm(TEMP.TYPE.LEVELS)
  
  # draw the "title" of the graph
  mtext("B", side=3, adj=0, line=1, cex=1.5)
  
  # write the date and time of plotting
  mtext(paste("Generated on:", format(Sys.time(), "%d %b %Y, %H:%M")),
        side=1, line=2, cex=1)
  
  # writes image to file
  dev.off()
  
  # add interactivity to the figure
  # execute only if parameter to show tooltips is TRUE
  if (generateTooltips) {
    library(SVGAnnotation)
    # open the generated SVG file
    doc <- xmlParse(fileName)
    
    # only generate graph with tooltips if the number of points is the same
    # as the number of tooltip annotations
    if (pointCounter!=length(getPlotPoints(doc)[[1]]))
    {errorMessages[length(errorMessages)+1] <<-
       paste("Critical Error: Number of points drawn (",
             length(getPlotPoints(doc)[[1]]),
             ") does not match the number",
             "of tooltips generated (",
             pointCounter,
             "). Graph with tooltips will not be generated.")
    } else {
      # add tool tips - must reference to the first list in 
      # a list of points - because they were drawn first? 
      # if the order of drawing changes, this will break
      addToolTips(getPlotPoints(doc)[[1]], 
                  symbolTooltip,
                  addArea=TRUE)
      # add CSS styles inside the file 
      # internal style enable easier sharing of SVG files
      # without having to share referenced CSS files
      addCSS(doc, insert=TRUE)
      saveXML(doc, fileName)
    }
  }
  
  # set default error message, if no errors were generated
  if (length(errorMessages) == 0) {
    errorMessages[length(errorMessages)+1] <<- "No errors found."
  }
}


############ HELPER FUNCTIONS ##################################################
# functions below are called by the plotTests function and are not meant to
# be called by the user

#' @title Draw labels and lines for groups
#' @description Function that plots labels for both possible groups describing
#' subjects.
#' @details
#' The function is meant to be called by the general function for plotting,
#' when it needs to draw the labels for the groups.
drawLevelLabels <- function (data, TYPE.LEVELS, LINE.SIZE, DIAGNOSIS.LEVELS,
                             firstLevel, secondLevel) {
  # get the rownumber for the first and last units in this 1st level group
  firstRowforType <- findFirstRowof1stLevel(data, firstLevel)
  lastRowforType <- findLastRowof1stLevel(data, firstLevel)
  # get rownumber for first and last unit in 2nd level group
  firstRowforDiagnosis <- findFirstRowof2ndLevel(data, firstLevel, secondLevel)
  lastRowforDiagnosis <- findLastRowof2ndLevel(data, firstLevel, secondLevel)
  # draw labels and lines for 2nd level
  draw2ndLevelLabels(label=secondLevel, firstRowforDiagnosis,
                     lastRowforDiagnosis )
  drawLineBelow2ndLevel(data, firstLevel, secondLevel,
                        lastRowforDiagnosis, DIAGNOSIS.LEVELS, LINE.SIZE)
  # draw labels and lines for 1st level
  draw1stLevelLabels(label=firstLevel, firstRowforType)
  drawLineBelow1stLevel(data, firstLevel, TYPE.LEVELS,
                        lastRowforType, LINE.SIZE)
}

#' @title Draws labels for the 2nd level groups
#' @description Not meant to be called by the user.
draw2ndLevelLabels <- function (label,
                                firstRowforDiagnosis,
                                lastRowforDiagnosis) {
  if (!is.na(label)){ mtext(line=1, text=label, side=2, las=2,  cex=.75,
                            padj=0, font=1,
                            at=(firstRowforDiagnosis+lastRowforDiagnosis)/2)}
  
  if (is.na(label)) {mtext(line=1, text="Unknown", side=2, las=2,  cex=.75,
                           padj=0, font=1,
                           at=(firstRowforDiagnosis+lastRowforDiagnosis)/2)}
}

#' @title Draws labels for the 1st level groups
#' @description Not meant to be called by the user.
draw1stLevelLabels <- function (label, firstRowforType ) {
  if (!is.na(label)){
    mtext(line=2, text=label, side=2, las=2,  cex=1, padj=0, font=2, 
          at=firstRowforType)}
  
  if (is.na(label)){
    mtext(line=2, text="Unknown", side=2, las=2,  cex=1, padj=0, font=2, 
          at=firstRowforType)}
  
}

#' @title Finds first row of 1st level group
#' @description Not meant to be called by the user.
findFirstRowof1stLevel <- function(data, firstLevel) {
  if (is.na(firstLevel))  # if 1st level is NA
  {min(data[is.na(data$Type),]["Order"], na.rm=TRUE)} else { 
    min(data[data$Type==firstLevel,]["Order"], na.rm=TRUE)} # if it is nonNA
}

#' @title Finds last row of 1st level group
#' @description Not meant to be called by the user.
findLastRowof1stLevel <- function (data, firstLevel) {
  if (is.na(firstLevel)) # if 1st level is NA
  {max(data[is.na(data$Type),]["Order"], na.rm=TRUE)} else { # if it is nonNA
    max(data[data$Type==firstLevel,]["Order"], na.rm=TRUE)}
}

#' @title Finds first row of 2nd level group
#' @description Not meant to be called by the user.
findFirstRowof2ndLevel <- function (data, firstLevel, secondLevel) {
  if ( is.na(firstLevel) & !is.na(secondLevel) ) # 1st level NA, 2nd level nonNA
  {x <- min(data[is.na(data$Type) & data$Diagnosis==secondLevel,]["Order"],
            na.rm=TRUE) } 
  
  if (is.na(firstLevel) & is.na(secondLevel)) # 1st level NA, 2nd level NA
  {x <- min(data[is.na(data$Type) & is.na(data$Diagnosis),]["Order"],
            na.rm=TRUE)} 
  
  if (!is.na(firstLevel) & !is.na(secondLevel)) # 1st level nonNA, 2nd level nonNA
  {x <- min(data[data$Type==firstLevel & data$Diagnosis==secondLevel,]["Order"],
            na.rm=TRUE)}
  
  if (!is.na(firstLevel) & is.na(secondLevel)) # 1st level nonNA, 2nd level NA
  {x <- min(data[data$Type==firstLevel & is.na(data$Diagnosis),]["Order"],
            na.rm=TRUE)}
  
  return(x)
}

#' @title Finds last row of 2nd level group
#' @description Not meant to be called by the user.
findLastRowof2ndLevel <- function (data, firstLevel, secondLevel) {
  if (is.na(firstLevel) & !is.na(secondLevel)) # 1st level NA, 2nd level nonNA
  {x <- max(data[is.na(data$Type) & data$Diagnosis==secondLevel,]["Order"],
            na.rm=TRUE) } 
  
  if (is.na(firstLevel) & is.na(secondLevel)) # 1st level NA, 2nd level NA
  {x <- max(data[is.na(data$Type) & is.na(data$Diagnosis),]["Order"],
            na.rm=TRUE)} 
  
  if (!is.na(firstLevel) & !is.na(secondLevel)) # 1st level nonNA, 2nd level nonNA
  {x <- max(data[data$Type==firstLevel & data$Diagnosis==secondLevel,]["Order"],
            na.rm=TRUE)}
  
  if (!is.na(firstLevel) & is.na(secondLevel)) # 1st level nonNA, 2nd level NA
  {x <- max(data[data$Type==firstLevel & is.na(data$Diagnosis),]["Order"],
            na.rm=TRUE)}
  return(x)
}

#' @title Draws lines above 1st level groups
#' @description Not meant to be called by the user.
drawLineAbove1stLevel <- function (firstRowforType, LINE.SIZE) {
  abline(h=firstRowforType-(LINE.SIZE/2), lty=2, col="black", lwd=2)    
}

#' @title Draws lines below 1st level groups
#' @description Not meant to be called by the user.
drawLineBelow1stLevel <- function (data,
                                   firstLevel,
                                   TYPE.LEVELS,
                                   lastRowforType,
                                   LINE.SIZE) {
  # if (firstLevel != tail(TYPE.LEVELS[!is.na(TYPE.LEVELS)], n=1)) {
  if (lastRowforType!=max(data$Order)){  
    abline(h=lastRowforType+(LINE.SIZE/2), lty=2, col="black", lwd=2) 
  }
}

#' @title Draws lines below 2nd level groups
#' @description Not meant to be called by the user.
drawLineBelow2ndLevel <- function (data,
                                   firstLevel,
                                   secondLevel,
                                   lastRowforDiagnosis,
                                   DIAGNOSIS.LEVELS,
                                   LINE.SIZE) {
  # add line after last unit, for any but the last group
  if (lastRowforDiagnosis!=max(data$Order)) {
    abline(h=lastRowforDiagnosis+(LINE.SIZE/2), lty=2, col="gray", lwd=2)
  }
  
  
}

#' @title Gets all test results written in a cell
#' @description Not meant to be called by the user.
#' @details
#' Parses test result strings from a string (a cell of data containing results).
#' Different separators can be defined. Leading spaces are removed.
isolateTests <- function (string, separator) {
  a <- unlist(strsplit(string, split=separator)) # isolate all tests in cell
  b <- sub(pattern="^ ", replacement="", x=a) # remove leading spaces
  return(b)  # return character vector to calling function
}


#' @title Adds an error message to error message list
#' @description Called from the plotTests function and not meant to be 
#' called by user.
#' @details 
#' This function adds an error message to the list of error messages
#' which is passed to RExcel. The messages are shown to the user 
#' in the Excel spreadsheet.
addErrorMessage <- function(text) {
  errorMessages[length(errorMessages)+1] <<- text
}

#' @title Checks if combination of groups exists
#' @description Checks if combination of groups exists and is not meant 
#' to be called from by user.
#' @details
#' Checks for existance of combination of first and second level groups
#' in the data.
checkifCombinationExists <- function (data, firstLevel, secondLevel) {
  if (!is.na(firstLevel) & !is.na(secondLevel)) {
    x <- (!dim(na.omit(data[data$Type==firstLevel &
                              data$Diagnosis==secondLevel,]["Order"]))[[1]]==0)
  }
  if (!is.na(firstLevel) &  is.na(secondLevel)) {
    x <- (!dim(na.omit(data[data$Type==firstLevel &
                              is.na(data$Diagnosis),]["Order"]))[[1]]==0)
  }
  if ( is.na(firstLevel) & !is.na(secondLevel)) {
    x <- (!dim(na.omit(data[is.na(data$Type) &
                              data$Diagnosis==secondLevel,]["Order"]))[[1]]==0)
  }
  if ( is.na(firstLevel) &  is.na(secondLevel)) {
    x <- (!dim(na.omit(data[is.na(data$Type) &
                              is.na(data$Diagnosis),]["Order"]))[[1]]==0)
  }
  return(x)
}

#' @title Plots a death of a patient with a special character
#' @description Not meant to be called by the user.
plotDeaths <- function (lineNumber, dayofDeath) {
  points(x=dayofDeath, y=lineNumber, pch=15, col="black", cex=2)
  # WARNING: for some pch values, the SVG file saves the plotted symbol
  # to a different XML node, which destroys the tooltip generation feature
  # safe pch are those with simple forms: circle, square, ...?
  # but not a cross or a star (since those are apparently drawn in more than one
  # move). Just so you know, in case that you change the pch value
  # and everything breaks :)
}

#' @title Sort results data
#' @description Function that sorts the data according to a criterion. Not to 
#' be called directly by the user.
#' @details
#' Function is called by the plotTests function, to sort the data before
#' it starts working on it. Different methods of sorting can be used.
#' "DateIn" sorts by the date of admission.
#' "BEA" uses seriation package and Bond Energy Algorithm.
#' "BEA_TSP" as above with TSP to optimize the measure of effectivenes.
#' "PCA" uses seriation package First principal component algorithm.
sortData <- function (data, sortMethod="BEA", 
                      nUNITS, 
                      DATES.COLUMN.FIRST,DATES.COLUMN.LAST,
                      TEST.RESULT.LEVELS ) {
  
  # if sorting method is by date of admission
  if (sortMethod=="DateIn") {
    # return data reordered by date of admission
    return(data[order(data$DateIn), ])
    
  } else { # else return data sorted by the seriation package
    nTESTS <- length(TEST.RESULT.LEVELS)
    nDATES <- (DATES.COLUMN.LAST - DATES.COLUMN.FIRST + 1)
    # initialize matrix to hold info about positive test results
    # the matrix has a row for every patient and
    # the columns for every day*result
    # and will contain 1 where the day+result is realized
    results <- matrix(0, nrow=nUNITS, ncol=nDATES*nTESTS)
    colnames(results) <- rep(TEST.RESULT.LEVELS, nDATES)
    for (i in 1:nUNITS) {
      for (ii in 1:nDATES) {
        for (iii in 1:nTESTS) {
          if(grepl(TEST.RESULT.LEVELS[iii],
                   data[i, DATES.COLUMN.FIRST + ii - 1])
          ) {
            results[i, (ii-1)*nTESTS + iii] <- 1         
          }
        }      
      }
    }
    # load library for sorting via similarity
    library(seriation)
    # sort, get order by patients dimension
    series <- seriate(results, method=sortMethod)
    seriesOrder <- get_order(series,dim=1)
    
    # return data resorted
    return(data[seriesOrder, ])
  }
}