generateDescription <- function(outputName) {

# GraphExpl_ProfilePlots_AllSubjects ####
if (outputName=="GraphExpl_ProfilePlots_AllSubjects") {
  description <- renderText({
paste(
"Data were displayed using profile plots (also known as spaghetti plots). 
Profile plots are scatterplots displaying the evaluation occasions and the values of the variables, where the values from the same subject are connected. The (bigger) red dots display the median values. Each outcome was displayed in a separate graph.<br><br>

Profile plots are useful for the identification of trends and to display individual changes.  When many subjects are plotted together, or the possible number of values of the variable is limited,  the profiles mighr overlap obscuring the trends. You might improve your graphs by displaying only a subset of the subject (select: Random selection of the subjects in one graph as the type of graph to plot) or by plotting multiple graphs for each outcome (select: Multiple graphs per outcome variable).  Lasagna plots might also be more informative.<br><br>

More descriptive statistics are provided in the Summary tabs. You can save or copy the graph(s) right clicking on them (selecting Copy Image or Save Image as...).<br><br>

Weiss RE (2005) Modeling Longitudinal Data: With 72 Figures. Springer.")
})}
  
# GraphExpl_ProfilePlots_RandomSubjects ####
if (outputName=="GraphExpl_ProfilePlots_RandomSubjects") {
  description <- renderText({
    paste(
"Data were displayed using profile plots (also known as spaghetti plots). 
Profile plots are scatterplots displaying the evaluation occasions and the values of the variables, where the values from the same subject are connected. The (bigger) red dots display the median values. Each outcome was displayed in a separate graph. A random subset of data was displayed in each graph.<br><br> 

Profile plots are useful for the identification of trends and to display individual changes.  When many subjects are plotted together, or the possible number of values of the variable is limited,  the profiles mighr overlap obscuring the trends. You might improve your graphs by varying the number of subjects to display (change the number of subject in the Select number of randomly selected subject in the menu),  by plotting multiple graphs for each outcome (select: Multiple graphs per outcome variable), or by plotting all the data  (select All subject in one graph). Lasagna plots might also be more informative.<br><br>

More descriptive statistics are provided in the Summary tabs. You can save or copy the graph(s) right clicking on them (selecting Copy Image or Save Image as...).<br><br>

Weiss RE (2005) Modeling Longitudinal Data: With 72 Figures. Springer.")
  })}

# GraphExpl_ProfilePlots_MultipleGraphs ####
if (outputName=="GraphExpl_ProfilePlots_MultipleGraphs") {
  description <- renderText({
    paste(
      "Data were displayed using profile plots (also known as spaghetti plots). 
Profile plots are scatterplots displaying the evaluation occasions and the values of the variables, where the values from the same subject are connected. The (bigger) red dots display the median values. Each outcome was displayed in a separate graph. Multiple graphs were obtained for each of the outcome (each graph display a subset of the subjects). <br><br>

Profile plots are useful for the identification of trends and to display individual changes.  When many subjects are plotted together, or the possible number of values of the variable is limited,  the profiles mighr overlap obscuring the trends. You might improve your graphs by varying the number of subjects to display in each grahph(change the number of subject in the Select number of randomly selected subject in the menu),  by plotting multiple graphs for each outcome (select: Multiple graphs per outcome variable), or by plotting all the data  (select All subject in one graph). Lasagna plots might also be more informative. <br><br>

More descriptive statistics are provided in the Summary tabs. You can save or copy the graph(s) right clicking on them (selecting Copy Image or Save Image as...). <br><br>

Weiss RE (2005) Modeling Longitudinal Data: With 72 Figures. Springer.
")
  })}

# GraphExpl_LasagnaPlots ####
if (outputName=="GraphExpl_LasagnaPlots") {
  description <- renderText({
    paste(
      "Data were displayed using heat maps (also known as lasagna plots). 
In lasagna plots the evaluation times are reported horizontally, as in the profile plots, but the measurements of each subject appear in the same row and colors are used to display the value of the variables. In our implementation the subjects are arranged using a hierarchical clustering algorithm (with Euclidean distance and complete linkage agglomeration method). The rearrangement of the subjects is useful for data exploration because similar subjects are grouped together. Missing values are displayed with white color.<br><br>

More descriptive statistics are provided in the Summary tabs. You can save or copy the graph(s) right clicking on them (selecting Copy Image or Save Image as...).<br><br>

Swihart B, Ca o B, James BD, Strand M, Schwartz BS, et al. (2010) Lasagna plots: a saucy alternative to spaghetti plots. Epidemiology 21: 621-625."
    )})}

# GraphExpl_BoxPlots ####
if (outputName=="GraphExpl_BoxPlots") {
  description <- renderText({
    paste(
      "Data were visualized using box and whisker plots (also known as boxplots). The boxplots represent the first quartile (i.e., lower edge of the box), median (i.e., bar inside the box), third quartile (i.e., upper edge of the box), and minimum and maximum (i.e., horizontal lines). If any points are at a greater distance from the quartiles than 1.5 times the interquartile range (IQR), these are plotted individually
and horizontal bars represent a distance of 1.5 times IQR from the upper or lower quartile.<br><br>

Boxplots do not display individual changes or the presence of missing values. Profile plots and heat maps (lasagna plots) might be used if the aim is to visualize individual changes over time.<br><br>

More descriptive statistics are provided in the Summary tabs. You can save or copy the graph(s) right clicking on them (selecting Copy Image or Save Image as...)."
    )})}

# GraphExpl_Timeline ####
if (outputName=="GraphExpl_Timeline") {
  description <- renderText({
    paste(
      "Data were visualized using a timeline plot. Timeline plots display the measurement occasion (date, evaluation occasion or day from inclusion) on the horizontal axis  while the measurements of each subject appear at the same height; the values of the outcomes are displayed using dots of different sizes (bubbles).  The colors denote the outcomes. <br><br>

All the selected outcomes and subjects are displayed in the same graph. 
Timeline plots are useful especially for small data sets, where the individual patterns are easy to follow or for displaying a small number of outcomes (you can remove the outcomes you are not interested from the sidebar).<br><br>

More descriptive statistics are provided in the Summary tabs. You can save or copy the graph(s) right clicking on them (selecting Copy Image or Save Image as...)."
    )})}

# GraphExpl_Barplots ####
if (outputName=="GraphExpl_Barplots") {
  description <- renderText({
    paste(
      "Data were visualized using barplots that display the proportion of patients with positive outcomes (with value of the binary outcome coded with the largest number in the data set) at a given evaluation occasion. <br><br>

You might want to display the proportions with their 95% confidence intervals, which are more informative. These graphs are available in the Summary tab.  <br><br>

More descriptive statistics are provided in the Summary tabs. You can save or copy the graph(s) right clicking on them (selecting Copy Image or Save Image as...)."
    )})}

##### Return value ######
return(description)
}