generateDescription <- function(outputName) {

# GraphExpl_ProfilePlots_AllSubjects ####
if (outputName=="GraphExpl_ProfilePlots_AllSubjects") {
  description <- renderText({
paste(
"Data were displayed using profile plots (also known as spaghetti plots). 
Profile plots are scatterplots displaying the evaluation occasions and the values of the variables, where the values from the same subject are connected. The (bigger) red dots display the median values. Each outcome was displayed in a separate graph.<br><br>

Profile plots are useful for the identification of trends and to display individual changes.  When many subjects are plotted together, or the possible number of values of the variable is limited,  the profiles might overlap obscuring the trends. You might improve your graphs by displaying only a subset of the subject (select: Random selection of the subjects in one graph as the type of graph to plot) or by plotting multiple graphs for each outcome (select: Multiple graphs per outcome variable).  Lasagna plots might also be more informative.<br><br>

More descriptive statistics are provided in the Summary tabs. You can save or copy the graph(s) right clicking on them (selecting Copy Image or Save Image as...).<br><br>

Weiss RE (2005) Modeling Longitudinal Data: With 72 Figures. Springer.")
})}
  
# GraphExpl_ProfilePlots_RandomSubjects ####
if (outputName=="GraphExpl_ProfilePlots_RandomSubjects") {
  description <- renderText({
    paste(
"Data were displayed using profile plots (also known as spaghetti plots). 
Profile plots are scatterplots displaying the evaluation occasions and the values of the variables, where the values from the same subject are connected. The (bigger) red dots display the median values. Each outcome was displayed in a separate graph. A random subset of data was displayed in each graph.<br><br> 

Profile plots are useful for the identification of trends and to display individual changes.  When many subjects are plotted together, or the possible number of values of the variable is limited,  the profiles might overlap obscuring the trends. You might improve your graphs by varying the number of subjects to display (change the number of subject in the Select number of randomly selected subject in the menu),  by plotting multiple graphs for each outcome (select: Multiple graphs per outcome variable), or by plotting all the data  (select All subject in one graph). Lasagna plots might also be more informative.<br><br>

More descriptive statistics are provided in the Summary tabs. You can save or copy the graph(s) right clicking on them (selecting Copy Image or Save Image as...).<br><br>

Weiss RE (2005) Modeling Longitudinal Data: With 72 Figures. Springer.")
  })}

# GraphExpl_ProfilePlots_MultipleGraphs ####
if (outputName=="GraphExpl_ProfilePlots_MultipleGraphs") {
  description <- renderText({
    paste(
      "Data were displayed using profile plots (also known as spaghetti plots). 
Profile plots are scatterplots displaying the evaluation occasions and the values of the variables, where the values from the same subject are connected. The (bigger) red dots display the median values. Each outcome was displayed in a separate graph. Multiple graphs were obtained for each of the outcome (each graph display a subset of the subjects). <br><br>

Profile plots are useful for the identification of trends and to display individual changes.  When many subjects are plotted together, or the possible number of values of the variable is limited,  the profiles might overlap obscuring the trends. You might improve your graphs by varying the number of subjects to display in each graph(change the number of subject in the Select number of randomly selected subject in the menu),  by plotting multiple graphs for each outcome (select: Multiple graphs per outcome variable), or by plotting all the data  (select All subject in one graph). Lasagna plots might also be more informative. <br><br>

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


# Summary_Medians ####
if (outputName=="Summary_Medians") {
  description <- renderText({
    paste(
"The table displays medians and interquartile ranges for the outcome variables. 95% confidence intervals are reported for the medians, based on the percentile bootstrap with 2000 iterations. The number of missing values for each outcome variable is reported.<br><br>

The graph shows the median values for the outcome variables along with their 95% confidence intervals."
    )})}

# Summary_Proportions ####
if (outputName=="Summary_Proportions") {
  description <- renderText({
    paste(
"The table displays the numbers and proportions of subjects with positive values of outcome variables. 95% confidence intervals are reported for proportions, based on the exact binomial method. The number of missing values for each outcome variable is reported.<br><br>

The graph shows the proportion of subjects with positive outcome values of outcome variables along with their 95% confidence intervals."
    )})}

# SummaryGrouping_Proportions ####
if (outputName=="SummaryGrouping_Proportions") {
  description <- renderText({
    paste(
"Table displays for each variable the proportion of subjects in a certain group, P value for the difference of proportions and the 95% confidence interval for the difference of proportions. The groups are compared with the chi-squared test with continuity correction. Data with missing values for grouping variable are removed from analysis.<br><br>

Adjusted P values and False discovery rates (Q values) taking into account multiple comparisons are calculated and displayed if the user chooses so. Adjusted P values are based on the Holm-Bonferroni method or multivariate permutation based adjustment. Q values are evaluated using the Benjamini-Hochberg or Benjamini-Hochberg-Yekutieli procedure.<br><br>

Westfall PH YS (1993) Resampling-Based Multiple Testing. Wiley New York.<br>
Benjamini Y, Hochberg Y (1995) Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society Series B (Methodological) : 289-300.<br>
Benjamini Y, Yekutieli D (2001) The control of the false discovery rate in multiple testing under dependency. Annals of statistics : 1165-1188."
    )})}

# SummaryGrouping_Medians ####
if (outputName=="SummaryGrouping_Medians") {
  description <- renderText({
    paste(
"Table displays for each variable the median value for subjects in a certain group, interquartile range for of the variable (25th to 75th percentile)and P value for the difference of samples. The groups are compared with the Mann-Whitney test. Data with missing values for grouping variable are removed from analysis. Threshold for positivity of variables is not taken into account.<br><br>

Adjusted P values and False discovery rates (Q values) taking into account multiple comparisons are calculated and displayed if the user chooses so. Adjusted P values are based on the Holm-Bonferroni method or multivariate permutation based adjustment. Q values are evaluated using the Benjamini-Hochberg or Benjamini-Hochberg-Yekutieli procedure.<br><br>

Westfall PH YS (1993) Resampling-Based Multiple Testing. Wiley New York.<br>
Benjamini Y, Hochberg Y (1995) Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society Series B (Methodological) : 289-300.<br>
Benjamini Y, Yekutieli D (2001) The control of the false discovery rate in multiple testing under dependency. Annals of statistics : 1165-1188."
    )})}

# Clustering_Dendrogram ####
if (outputName=="Clustering_Dendrogram") {
  description <- renderText({
    paste(
"The dendrogram displays the similarity of subjects by hierarchically clustering the outcomes for the chosen evaluation occasion."
    )})}

# Clustering_Heatmap ####
if (outputName=="Clustering_Heatmap") {
  description <- renderText({
    paste(
"The heat map displays the complete data obtained at the chosen evaluation occasion. A column represents values for a particular subject, while the rows represent outcome variables. Values are color coded. "
    )})}

# Clustering_Correlations ####
if (outputName=="Clustering_Correlations") {
  description <- renderText({
    paste(
"The heat map displays correlations between outcome variables at the chosen evaluation occasion. Values of pairwise Spearman correlations between two outcome variables are also displayed numerically in each cell. "
    )})}

# RegressionOne_OddsRatio ####
if (outputName=="RegressionOne_OddsRatio") {
  description <- renderText({
    paste(
"The results in the tables display odds ratios and odds intercepts for outcome variables estimated using logistic regression. The odds ratios are expressed for a level of covariate compared to the reference level of the covariate. The tables also display 95% confidence intervals for these parameters along with the P values. <br><br>

The graph displays the odds ratios together with the 95% confidence intervals."
    )})}

# RegressionOne_Linear ####
if (outputName=="RegressionOne_Linear") {
  description <- renderText({
    paste(
"The results in the tables display beta regression coefficients and intercepts for outcome variables. The coefficients represent the change in the outcome for a one unit change of the covariate. The tables also display 95% confidence intervals for these parameters along with the P values. <br><br>

The graph displays the estimated coefficient values together with their 95% confidence intervals."
    )})}

# RegressionOne_Firth ####
if (outputName=="RegressionOne_Firth") {
  description <- renderText({
    paste(
"The results in the tables display odds ratios and odds intercepts for outcome variables estimated using logistic regression with the Firth correction. The odds ratios are expressed for a level of covariate compared to the reference level of the covariate. The tables also display 95% confidence intervals for these parameters along with the P values. <br><br>

The graph displays the odds ratios together with the 95% confidence intervals.<br><br>

Heinze G, Schemper M (2002) A solution to the problem of separation in logistic regression. Statistics in medicine 21: 2409-2419."
    )})}

# RegressionOne_RCS ####
if (outputName=="RegressionOne_RCS") {
  description <- renderText({
    paste(
"The graphs represent modeling the association of the outcome variables with the chosen covariate using restricted cubic splines. Non linear associations might be apparent from the graphs. <br><br>

Harrel FE, Lee KL, Pollock BG (1988) Regression models in clinical studies: determining relationships between predictors and response. Journal of the National Cancer Institute 80: 1198-1202."
    )})}


# RegressionAll ####
if (outputName=="RegressionAll") {
  description <- renderText({
    paste(
"Modelling is done for each outcome variable separately, but over all evaluation occasions while allowing the intercept to vary for each subject (using a mixed model). Tables display the estimated intercepts for the models and the estimated regression coefficients (either odds ratios for binary or beta coefficients for numerical and categorical variables). The graphs display the estimated coefficients along with their 95% confidence intervals.<br><br>

Gelman A, Hill J (2006) Data analysis using regression and multilevel/hierarchical models. Cambridge University Press."
    )})}

##### Return value ######
return(description)
}