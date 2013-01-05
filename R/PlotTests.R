# file containing function to draw data
# work in progress - content being created from figuresFinalNov11.r
# as this seems to be the latest version of code

# Will use coding conventions as described in :
# https://docs.google.com/document/edit?id=1esDVxyWvH8AsX-VJa-8oqWaHLs4stGlIbk8kLc5VlII

### DRAW FIGURES FUNCTION ####

# TODO: study the seriation package (if there is a lot of heterogeneity with the date of 
## first arrival, let it be sorted by the date of first arrival, otherwise try to use
## seriation package)
# TODO: add checking if all received results values are valid. otherwise return an error to Exceland abort.
# TODO: add a static picture to be generated
# TODO: če je več skupini in je ena NA, naj zanjo izpiše "Unknown" sicer naj ne izpiše nič, če je NA edina skupina
# TODO: naredi presledke med skupinami na spodnjem grafu večje - npr. 1/3 ali pa 1/2 vsega prostora
# TODO: probaj sortirati absolutno in po vsaki skupini posebej; recimo za paciente je datum mogoče čisto v redu, za osebje pa verjetno kaj drugega
# TODO: premisli, kako bi naredil, da bi SVGAnnotation deloval: kaj če bi "points" zagnal v for loopu za vsak level posebej  - bi potem lažje uporabil SVGAnnotation ?


plotTests <- function (data, figureParameters, graphsDir = getwd(),
                       generateTooltips = TRUE) {
  # start profiling
  Rprof(filename="Rprof.out", append=TRUE)
  
  # initialize placeholder for error messages 
  errorMessages <<- list()
  
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
  
  
  # identify and store first and last columns that contain dates of tests ####
  datesColumnsIndices <- grep("^[0-9]{1,2}[.][0-9]{1,2}[.][0-9]{4}$",
                              colnames(data))
  DATES.COLUMN.FIRST <- min(datesColumnsIndices)
  DATES.COLUMN.LAST <- max(datesColumnsIndices)
  
  # identify and store parameters for drawing (levels of test results, color, ####
  # size of symbols) 
  TEST.RESULT.LEVELS <- as.character(unlist(figureParameters["Result"]))
  DOT.COLORS <- as.character(unlist(figureParameters["Color"]))
  DOT.SIZES <- as.integer(unlist(figureParameters["Size"]))
  
  # TODO: add checking if all levels in Data are valid levels also listed in Parameters
  
  # make dot colors transparent with transparency APLHA (0-255) ####
  ALPHA <- 150
  DOT.COLORS <- apply(col2rgb(DOT.COLORS), 2, function(x)
  {rgb(red=x[1], green=x[2], blue=x[3], alpha=ALPHA, maxColorValue=255)} )
  
  # set the color for the lines at unit level ####
  LINE.UNIT.COLOR <- rgb(red=col2rgb("gray")[1], 
                         green=col2rgb("gray")[2],
                         blue=col2rgb("gray")[3],
                         alpha=ALPHA,
                         maxColorValue=255)
  
  # count the number of units in sample
  nUNITS <- dim(data)[1]
  
  # store the dates in R date format
  datesofTests <- as.Date(colnames(data)[datesColumnsIndices], "%d.%m.%Y")
  
  # vector of relative difference in days from test on day x to test on day 1
  daysofTests <- datesofTests-datesofTests[1]
  
  # create table of frequencies for all USED combinations of types & diagnosistabl
  tableofGroups <- table(data$Type, data$Diagnosis, useNA="ifany")
  
  # TODO: kako imenovati 1st levele in 2nd level? -> uporabniki
  
  # walk through all the units in the sample and assign them 
  # an absolute line number (keeping spaces for separating lines between groups)
  nTYPES <- dim(tableofGroups)[1]
  nDIAGNOSIS <- dim(tableofGroups)[2]
  TYPE.LEVELS <- dimnames(tableofGroups)[[1]]
  DIAGNOSIS.LEVELS <- dimnames(tableofGroups)[[2]]
  
  # set size of space for separating lines on graph
  LINE.SIZE <- 4
  
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
          data[which(is.na(data$Type) & is.na(data$Diagnosis)),]["Order"] <- (lineNo):(lineNo-1 + cellSize)
          # increase line number by cellsize and buffer for the separating line
          lineNo <- lineNo + cellSize + LINE.SIZE
          
          # case when only 1st level is NA
        } else {
          # assign an abolute order number to units in this cell
          data[which(is.na(data$Type) & data$Diagnosis==diagnosisName),]["Order"] <- (lineNo):(lineNo-1 + cellSize)
          # increase line number by cellsize and buffer for the separating line
          lineNo <- lineNo + cellSize + LINE.SIZE
        }
      } else {
        # catch a special case when only 2nd level is NA
        if (is.na(diagnosisName)) {
          data[which(data$Type==typeName & is.na(data$Diagnosis)),]["Order"] <- (lineNo):(lineNo-1 + cellSize)
          # increase line number by cellsize and buffer for the separating line
          lineNo <- lineNo + cellSize + LINE.SIZE
        } else {
          # assign an abolute order number to units in this cell
          data[which(data$Type==typeName & data$Diagnosis==diagnosisName),]["Order"] <- (lineNo):(lineNo-1 + cellSize)
          # increase line number by cellsize and buffer for the separating line
          lineNo <- lineNo + cellSize + LINE.SIZE
        }
      }
    }
  }
  
  # function to get all the test results from a cell
  isolateTests <- function (string, separator) {
    a <- unlist(strsplit(string, split=separator)) # isolate all tests in cell
    b <- sub(pattern="^ ", replacement="", x=a) # remove leading spaces
    return(b)  # return character vector to calling function
  }
  
  # load plotting library ####
  library(Cairo)
  
  Cairo(paste(graphsDir, "/", "example.svg", sep=""), type="svg",width=max(max(daysofTests)/4,10 ),height=29,units="cm",dpi=300)
  # Cairo("example.pdf", type="pdf",width=19,height=24,units="cm",dpi=300)
  # svg("example1.svg")
  
  #set layout
  layout(c(1,2), heights=c(4,1))
  
  # set margins for first figure
  par(mar=c(5, 7, 4, 2) + 0.1 )
  
  
  # calculate buffer for y axis size (account for drawing lines between groups)
  
  # debugging
  podatki <<- data
  browser()

  
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
        data[data$Order==i, DATES.COLUMN.FIRST : DATES.COLUMN.LAST][ii]))
                            , separator=",")
      
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
        # if (k>1) browser()
        symbolTooltip[pointCounter+k] <-  
          paste("ID:", data[data$Order==i, "ID"], # "ID" of patient     
                ", positive:", # list tests which are positive
                as.character(unlist(data[data$Order==i,
                                         DATES.COLUMN.FIRST : 
                                           DATES.COLUMN.LAST][ii])),
                ", date:", format(datesofTests[ii],format="%d.%m.%y" )# Date of test
                
          )
      }
      
      # increment counter of symbols drawn
      pointCounter <- pointCounter + length(tests)
      
      
      
      
    }
  }
  
  
  
  # TODO: should dying as an outcome be plotted? 
  # ANSW: make it a parameter - which is the special event that is marked with X, add a column with date of event
  
  # draw labels on the x axis ####
  axis(1, at=daysofTests, labels=rep("", length(daysofTests)), cex.axis=.35)
  DATE.LABELS <- format(datesofTests, format="%d %b")
  axis(1, at=daysofTests[seq(1, length(daysofTests), 2)], 
       labels=DATE.LABELS[seq(1, length(daysofTests), 2)], 
       cex.axis=.75, line=-0.5, lwd=0)
  axis(1, at=daysofTests[seq(2, length(daysofTests), 2)], 
       labels=DATE.LABELS[seq(2, length(daysofTests), 2)],
       cex.axis=.75, line=-1, lwd=0)
  
  # draw horizontal lines for each unit ####
  abline(h=unlist(data["Order"]), lty=2, col=LINE.UNIT.COLOR)
  
  
  # add labels for first level groups ####
  # function to draw 1st level labels  
  drawLevelLabels <- function (firstLevel, secondLevel) {
    # 1st level draw label for nonNA case
    if (!is.na(firstLevel)) {
      # get the rownumber for the first unit in this first level group
      firstRowforType <- 
        min(data[data$Type==firstLevel,]["Order"], na.rm=TRUE)
      
      # and write the name of the type on the margin
      mtext(line=2, text=firstLevel, side=2, las=2,  cex=1, padj=0, font=2, 
            at=firstRowforType)
      
      # get row number for last unit in this first level group
      lastRowforType <- 
        max(data[data$Type==firstLevel,]["Order"], na.rm=TRUE)
      
      # add line after last unit, for any but the last group - case with NA values
      if (firstLevel != tail(TYPE.LEVELS[!is.na(TYPE.LEVELS)], n=1)) {
        abline(h=lastRowforType+(LINE.SIZE/2), lty=2, col="black", lwd=2) 
      }
      
      # 2nd level draw label
      # only look at existing 1st and 2nd level combinations
      # TODO: take this line out and call this function only for existing combinations???
      if (!dim(na.omit(data[data$Type==firstLevel &
                              data$Diagnosis==secondLevel,]["Order"]))[[1]]==0) {
        
        # 2nd level nonNA scenario ####
        if (!is.na(secondLevel)) {
          # get the rownumber for the first and last unit in this 2nd level group
          firstRowforDiagnosis <- 
            min(data[data$Type==firstLevel & data$Diagnosis==secondLevel,]["Order"],
                na.rm=TRUE)
          lastRowforDiagnosis <- 
            max(data[data$Type==firstLevel & data$Diagnosis==secondLevel,]["Order"],
                na.rm=TRUE)
          
          # and write the name of the type on the margin in the middle of group
          mtext(line=1, text=secondLevel, side=2, las=2,  cex=.75, padj=0, font=1,
                at=(firstRowforDiagnosis+lastRowforDiagnosis)/2)
          
          # add line after last unit, for any but the last group
          existingLevels <- as.character(unlist((
            na.omit(unique(data[data$Type==firstLevel,]["Diagnosis"])))))
          existingLevelsSorted <- DIAGNOSIS.LEVELS[sort(match(DIAGNOSIS.LEVELS,existingLevels), na.last=NA)]
          
          if (secondLevel != tail( existingLevelsSorted, n=1 )) {
            abline(h=lastRowforDiagnosis+(LINE.SIZE/2), lty=2, col="gray", lwd=2)
          }
        }
      }
    }
    
    # 1st level draw label for NA case
    if (is.na(firstLevel)) {
      # get the rownumber for the first unit in this first level group
      firstRowforType <- 
        min(data[is.na(data$Type),]["Order"], na.rm=TRUE)
      
      # and write the name of the type on the margin
      mtext(line=2, text="Unknown", side=2, las=2,  cex=1, padj=0, font=2, 
            at=firstRowforType)
      
      # get row number for last unit in this first level group
      lastRowforType <- 
        max(data[is.na(data$Type),]["Order"], na.rm=TRUE)
      
      # add line above category
      abline(h=firstRowforType-(LINE.SIZE/2), lty=2, col="black", lwd=2)    
      
      # 2nd level draw label for nonNA case
      # TODO: take this line out and call this function only for existing combinations???
      if (!dim(na.omit(data[is.na(data$Type) &
                              data$Diagnosis==secondLevel,]["Order"]))[[1]]==0) {
        
        # 2nd level nonNA scenario ####
        if (!is.na(secondLevel)) {
          # get the rownumber for the first and last unit in this 2nd level group
          firstRowforDiagnosis <- 
            min(data[is.na(data$Type) & data$Diagnosis==secondLevel,]["Order"],
                na.rm=TRUE)
          lastRowforDiagnosis <- 
            max(data[is.na(data$Type) & data$Diagnosis==secondLevel,]["Order"],
                na.rm=TRUE)
          
          # and write the name of the type on the margin in the middle of group
          mtext(line=1, text=secondLevel, side=2, las=2,  cex=.75, padj=0, font=1,
                at=(firstRowforDiagnosis+lastRowforDiagnosis)/2)
          
          # add line after last unit, for any but the last group
          # TODO: check if lines are correctly dawn, when there are no NA values!    
          existingLevels <- as.character(unlist((
            na.omit(unique(data[is.na(data$Type),]["Diagnosis"])))))
          existingLevelsSorted <- DIAGNOSIS.LEVELS[sort(match(existingLevels,DIAGNOSIS.LEVELS), na.last=NA)]
          if (secondLevel != tail(existingLevelsSorted, n=1)) {
            abline(h=lastRowforDiagnosis+(LINE.SIZE/2), lty=2, col="gray", lwd=2)
          }
        }
      }
      
      # 2nd level draw label for NA case
      
      if (!dim(na.omit(data[is.na(data$Type) &
                              is.na(data$Diagnosis),]["Order"]))[[1]]==0) {
        
        # 2nd level nonNA scenario ####
        if (!is.na(secondLevel)) {
          # get the rownumber for the first and last unit in this 2nd level group
          firstRowforDiagnosis <- 
            min(data[is.na(data$Type) & is.na(data$Diagnosis),]["Order"],
                na.rm=TRUE)
          lastRowforDiagnosis <- 
            max(data[is.na(data$Type) & is.na(data$Diagnosis),]["Order"],
                na.rm=TRUE)
          
          # and write the name of the type on the margin in the middle of group
          mtext(line=1, text="", side=2, las=2,  cex=.75, padj=0, font=1,
                at=(firstRowforDiagnosis+lastRowforDiagnosis)/2)
          
          # add line above category
          abline(h=firstRowforDiagnosis-(LINE.SIZE/2), lty=2, col="gray", lwd=2)
          
        }
      }
      
    }
  }
  
  # call function to draw 1st level labels
  #  for (i in TYPE.LEVELS) {
  #  draw1stLevelLabel(firstLevel=i)
  #  }
  
  
  
  
  # call function to draw labels for each 1st+2nd level combination ####
  for (i in TYPE.LEVELS) {
    for (ii in DIAGNOSIS.LEVELS) {
      drawLevelLabels(firstLevel=i, secondLevel=ii)
    }
  }
  
  
  
  # add legend - what test is meant by each color ####
  # TODO: add dots (symbols) to the legend, above the labels
  for(i in 1:length(TEST.RESULT.LEVELS)) {
    mtext(line=2, text=TEST.RESULT.LEVELS[i], side=3, las=1,  cex=.75, padj=1, font=2, at=32+i*5-5, col=DOT.COLORS[i])
  }
  
  # add label of graph
  mtext("A", side=3, adj=0, line=1, cex=1.5)
  
  
  
  #### draw the second figure #####
  # TODO: the second figure draws thepercentage of infected in each 1st level group
  # but how should this be implemented ?
  # should the negative results be hardcoded as "neg" (or meybe better "negative"?)
  # ANSW: harcode/ keep the first line in Excel as "negative"
  # ANSW: think about possiblity of drawing confidence interval inste OR in addition to 
  # column; should the user have a choice of drawing only means and also confindence intervals
  # should the percentage of positive be (positive)/(positive + negative)* 100%
  # or maybe (positive)/(all) (I guess not, it makes little sense to do this)
  # ANSW : we look only at the nonmissing values
  
  
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
        sumPositive <- sum(!is.na(data[data$Type==ii, i]) & data[data$Type==ii, i]!="neg" ) 
        positiveUnits[row, column] <- sumPositive/sumNonNA*100
      }
      # for case when 1st level is NA
      if (is.na(ii)) {
        sumNonNA <- sum(!is.na(data[is.na(data$Type), i]))
        sumPositive <- sum(!is.na(data[is.na(data$Type), i]) & data[is.na(data$Type), i]!="neg" ) 
        positiveUnits[row, column] <- sumPositive/sumNonNA*100
      }
      row <- row + 1
    }
    
    column <- column +1 
  }
  
  # calculate minumum difference between tests in days
  minimumInterval <- min(as.numeric(diff(daysofTests)))
  # calculate space available to draw bar, 
  #leaving a buffer the size of one width
  barWidth <- minimumInterval / (length(TYPE.LEVELS)+1)
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
      } else { # all columns inside a group have zero space between
        spacingVector[i] <- 0
      }
    }
  }
  
  # TODO: should colors for groups be parameters for the user to set?
  # TODO: at least set them for a general case of N 1st levels
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
  # draw labels on the x axis ####
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
  
  # draw legend
  # TODO: generalise to write used labels and used colors
  legend("topright",  legend=TYPE.LEVELS, fill=legendColors, cex=.7)
  
  # draw the "title" of the graph
  mtext("B", side=3, adj=0, line=1, cex=1.5)
  
  
  # writes image to file
  dev.off()
  
  
  
  ### add interactivity to the figure ####
  # execute only if parameter to show tooltips is TRUE
  if (generateTooltips) {
    library(SVGAnnotation)
    # open the generated SVG file
    doc <- xmlParse(paste(graphsDir, "/", "example.svg", sep=""))
    # TODO: make a constant to reference the file (instead of using paste everywhere as above)
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
    saveXML(doc, paste(graphsDir, "/", "example_tooltips.svg", sep=""))
  }
  
  # TODO: take out functions for profiling
  # summaryRprof(filename="Rprof.out")
  # parse_rprof("./scripts/Rprof.out")
  # ggplot.profr("./scripts/Rprof.out")
  # plot.profr("./scripts/Rprof.out")
  # plot(parse_rprof("./scripts/Rprof.out"))
  
  
  # set default error message, if no errors were generated
  if (length(errorMessages) == 0) {
    errorMessages[length(errorMessages)+1] <<- "No errors found."
  }
  
  # for debuging
  pointsPlottedWithTooltips <<- symbolTooltip
  pointPlottedSVG <<- getPlotPoints(doc)[[1]]
  
}
