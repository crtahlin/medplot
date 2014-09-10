generateDescription <- function(outputName) {

# GraphExpl_ProfilePlots_AllSubjects ####
if (outputName=="GraphExpl_ProfilePlots_AllSubjects") {
  description <- renderText({
paste(
"Data were displayed using profile plots (also known as spaghetti plots). 
Profile plots are scatterplots displaying the evaluation occasions and the values of the variables where the values from the same subject are connected. The larger red dots display the median values. Each outcome was displayed in a separate graph.<br><br>

Profile plots are useful for the identification of trends and to display individual changes.  When many subjects are plotted together or the possible number of values of the variable is limited, the profiles might overlap obscuring the trends. You might improve your graphs by displaying only a subset of the subjects (select: 'Random selection of the subjects on one graph' as the type of graph to plot) or by plotting multiple graphs for each outcome (select: 'Multiple graphs per outcome variable').  You could also try ploting Lasagna plots, as they might be more informative.<br><br>

More descriptive statistics are provided in the Summary tabs.<br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button.<br><br>

References: <br>
Weiss RE (2005) Modeling Longitudinal Data: With 72 Figures. Springer.")
})}
  
# GraphExpl_ProfilePlots_RandomSubjects ####
if (outputName=="GraphExpl_ProfilePlots_RandomSubjects") {
  description <- renderText({
    paste(
      "Data were displayed using profile plots (also known as spaghetti plots). 
Profile plots are scatterplots displaying the evaluation occasions and the values of the variables where the values from the same subject are connected. The larger red dots display the median values. Each outcome was displayed in a separate graph.<br><br>

Profile plots are useful for the identification of trends and to display individual changes.  When many subjects are plotted together or the possible number of values of the variable is limited, the profiles might overlap obscuring the trends. You might improve your graphs by displaying only a subset of the subjects (select: 'Random selection of the subjects on one graph' as the type of graph to plot) or by plotting multiple graphs for each outcome (select: 'Multiple graphs per outcome variable').  You could also try ploting Lasagna plots, as they might be more informative.<br><br>

More descriptive statistics are provided in the Summary tabs.<br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button.<br><br>

References: <br>
Weiss RE (2005) Modeling Longitudinal Data: With 72 Figures. Springer.")
  })}

# GraphExpl_ProfilePlots_MultipleGraphs ####
if (outputName=="GraphExpl_ProfilePlots_MultipleGraphs") {
  description <- renderText({
    paste(
"Data were displayed using profile plots (also known as spaghetti plots). 
Profile plots are scatterplots displaying the evaluation occasions and the values of the variables where the values from the same subject are connected. The larger red dots display the median values. Each outcome was displayed in a separate graph.<br><br>

Profile plots are useful for the identification of trends and to display individual changes.  When many subjects are plotted together or the possible number of values of the variable is limited, the profiles might overlap obscuring the trends. You might improve your graphs by displaying only a subset of the subjects (select: 'Random selection of the subjects on one graph' as the type of graph to plot) or by plotting multiple graphs for each outcome (select: 'Multiple graphs per outcome variable').  You could also try ploting Lasagna plots, as they might be more informative.<br><br>

More descriptive statistics are provided in the Summary tabs.<br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button.<br><br>

References: <br>
Weiss RE (2005) Modeling Longitudinal Data: With 72 Figures. Springer.")
  })}

# GraphExpl_LasagnaPlots ####
if (outputName=="GraphExpl_LasagnaPlots") {
  description <- renderText({
    paste(
      
"Data were displayed using heat maps (also known as lasagna plots). 
In lasagna plots the evaluation times are reported horizontaly and the measurements of each subject appear in the same row. Colors are used to display the value of the variables. In our implementation the subjects are arranged using a hierarchical clustering algorithm (with Euclidean distance and complete linkage agglomeration method). The rearrangement of the subjects is useful for data exploration because similar subjects are grouped together. Missing values are displayed with white color.<br><br>

More descriptive statistics are provided in the Summary tabs. <br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button.<br><br>

References: <br>
Swihart B, Ca o B, James BD, Strand M, Schwartz BS, et al. (2010) Lasagna plots: a saucy alternative to spaghetti plots. Epidemiology 21: 621-625."
    )})}

# GraphExpl_BoxPlots ####
if (outputName=="GraphExpl_BoxPlots") {
  description <- renderText({
    paste(

"Data were visualized using box and whisker plots (also known as boxplots). The boxplots represent the first quartile (i.e., lower edge of the box), median (i.e., bar inside the box), third quartile (i.e., upper edge of the box), and minimum and maximum (i.e., horizontal lines). If any points are at a greater distance from the quartiles than 1.5 times the interquartile range (IQR), these are plotted individually as dots. Horizontal bars represent a distance of 1.5 times IQR from the upper or lower quartile.<br><br>

Boxplots do not display individual changes or the presence of missing values. Profile plots and heat maps (lasagna plots) might be used if the aim is to visualize individual changes over time.<br><br>

More descriptive statistics are provided in the Summary tabs.<br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button."
    )})}

# GraphExpl_Timeline ####
if (outputName=="GraphExpl_Timeline") {
  description <- renderText({
    paste(

"Data were visualized using a timeline plot. Timeline plots display the measurement occasion (date, evaluation occasion or day from inclusion) on the horizontal axis while the measurements of each subject appear at the same height; the values of the outcomes are displayed using dots of different sizes (bubbles).  The colors denote the outcomes. All the selected outcomes and subjects are displayed in the same graph. <br><br>

Timeline plots are useful especially for small data sets, where the individual patterns are easy to follow or for displaying a small number of outcomes (you can use the menu to de-select the outcomes you are not interested in).<br><br>

More descriptive statistics are provided in the Summary tabs.<br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button."
    )
  })}

# GraphExpl_Barplots ####
if (outputName=="GraphExpl_Barplots") {
  description <- renderText({
    paste(

"Data were visualized using barplots that display the proportion of patients with positive outcomes at a given evaluation occasion. <br><br>

You might want to display the proportions with their 95% confidence intervals, which are more informative. These graphs are available in the Summary tab. <br><br>

More descriptive statistics are provided in the Summary tabs.<br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button."
    )})}


# Summary_Medians ####
if (outputName=="Summary_Medians") {
  description <- renderText({
    paste(
      
"The table displays estimated medians and interquartile ranges for the outcome variables. 95% confidence intervals are reported for the medians, based on the percentile bootstrap with 2000 iterations. The number of missing values for each outcome variable is reported.<br><br>

The graph shows the estimated median values for the outcome variables along with their 95% confidence intervals.<br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button."
    )})}

# Summary_Proportions ####
if (outputName=="Summary_Proportions") {
  description <- renderText({
    paste(
      
"
The first graph shows the estimated proportion of subjects with positive values of outcome variables. All evaluation occasion are represented on the graph - with horizontal bars of different color stacked on top of each other. Both groups are represented on the graph - one to the left and the other to right of the zero value on the horizontal axis.

The table displays the frequencies and estimated proportions of subjects with positive values of outcome variables. 95% confidence intervals are reported for proportions, based on the exact binomial method. The number of missing values for each outcome variable is reported.<br><br>

The second graph shows the estimated proportion of subjects with positive outcome values of outcome variables along with their 95% confidence intervals.<br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button."
    )})}

# SummaryGrouping_Proportions ####
if (outputName=="SummaryGrouping_Proportions") {
  description <- renderText({
    paste(
"
The graph shows the estimated proportion of subjects with positive values of outcome variables together with their 95% confidence intervals. All evaluation occasions are represented for each outcome variable. Both groups are represented on the graph - one to the left and the other to right of the zero value on the horizontal axis.

The table displays the estimated proportion of subjects in a certain group, P value for the difference of proportions and the 95% confidence interval for the difference of proportions. The groups are compared using the chi-squared test with continuity correction. Data with missing values for grouping variable are removed from analysis.<br><br>

Adjusted P values and False discovery rates (Q values) taking into account multiple comparisons are calculated and displayed if the user chooses so. Adjusted P values are based on the Holm-Bonferroni method (conservative and lacks statistical power with correlated if the outcomes are correlated) or multivariate permutation based adjustment (takes into account the correlation between outcomes and is generaly more statisticaly powerful than Holm-Bonferroni).  Q values are evaluated using the Benjamini-Hochberg (assumes inedependent or positively dependent outcomes) or Benjamini-Hochberg-Yekutieli procedure (makes no assumptions about outcome dependence but is more conservative). Q values represent the minimum false discovery rate at which the test may be called significant.<br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button.<br><br>

References:<br>
Westfall PH YS (1993) Resampling-Based Multiple Testing. Wiley New York.<br>
Benjamini Y, Hochberg Y (1995) Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society Series B (Methodological) : 289-300.<br>
Benjamini Y, Yekutieli D (2001) The control of the false discovery rate in multiple testing under dependency. Annals of statistics : 1165-1188."
    )})}

# SummaryGrouping_Medians ####
if (outputName=="SummaryGrouping_Medians") {
  description <- renderText({
    paste(
"The table displays the median value for subjects in a certain group, interquartile range for the variable (25th to 75th percentile) and P value for the difference of samples. The groups are compared with the Mann-Whitney test. Data with missing values for grouping variable are removed from analysis. Threshold for positivity of variables is not taken into account.<br><br>

Adjusted P values and False discovery rates (Q values) taking into account multiple comparisons are calculated and displayed if the user chooses so. Adjusted P values are based on the Holm-Bonferroni method (conservative and lacks statistical power with correlated if the outcomes are correlated) or multivariate permutation based adjustment (takes into account the correlation between outcomes and is generaly more statisticaly powerful than Holm-Bonferroni).  Q values are evaluated using the Benjamini-Hochberg (assumes inedependent or positively dependent outcomes) or Benjamini-Hochberg-Yekutieli procedure (makes no assumptions about outcome dependence but is more conservative). Q values represent the minimum false discovery rate at which the test may be called significant.<br><br>

References:<br>
Westfall PH YS (1993) Resampling-Based Multiple Testing. Wiley New York.<br>
Benjamini Y, Hochberg Y (1995) Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society Series B (Methodological) : 289-300.<br>
Benjamini Y, Yekutieli D (2001) The control of the false discovery rate in multiple testing under dependency. Annals of statistics : 1165-1188."
    )})}

# Clustering_Dendrogram ####
if (outputName=="Clustering_Dendrogram") {
  description <- renderText({
    paste(
"The dendrogram displays the similarity of subjects by hierarchically clustering the outcomes for the chosen evaluation occasion. For numerical variables, their corelations are used in computing their differences. For binary variables, Euclidean distance is used. <br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button."
    )})}

# Clustering_Heatmap ####
if (outputName=="Clustering_Heatmap") {
  description <- renderText({
    paste(
"The heat map displays the complete data obtained at the chosen evaluation occasion. A column represents values for a particular subject, while the rows represent outcome variables. Subjects and outcome variables are arranged according to their similarities using hierachical clustering with complete linkage method and Euclidean distance. Values of outcomes are color coded.<br><br>

Additional variables can be selected to annotate the graph - their values are displayed in the top row.<br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button.
"
    )})}

# Clustering_Correlations ####
if (outputName=="Clustering_Correlations") {
  description <- renderText({
    paste(
"The heat map displays correlations between outcome variables at the chosen evaluation occasion. Values of pairwise Spearman correlations between two outcome variables are also displayed numerically in each cell and color coded. Only complete observations are used. The outcome variables are arranged according to their similarities using hierachical clustering with complete linkage method and Euclidean distance.<br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button."
    )})}

# RegressionOne_OddsRatio ####
if (outputName=="RegressionOne_OddsRatio") {
  description <- renderText({
    paste(
"The results in the tables display estimated odds ratios and odds intercepts for outcome variables estimated using logistic regression. The odds ratios are expressed for a level of covariate compared to the reference level of the covariate. The tables also display 95% confidence intervals for these parameters along with their P values. <br><br>

The graph displays the odds ratios together with the 95% confidence intervals.<br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button."
    )})}

# RegressionOne_Linear ####
if (outputName=="RegressionOne_Linear") {
  description <- renderText({
    paste(
"The results in the tables display estimated beta regression coefficients and intercepts for outcome variables. The coefficients represent the change in the outcome for a one unit change of the covariate. The tables also display 95% confidence intervals for these parameters along with their P values. <br><br>

The graph displays the estimated coefficient values together with their 95% confidence intervals. <br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button."
    )})}

# RegressionOne_Firth ####
if (outputName=="RegressionOne_Firth") {
  description <- renderText({
    paste(
"The results in the tables display estimated odds ratios and odds intercepts for outcome variables estimated using logistic regression with the Firth correction. Zhe Firth correction is useful for small data sets or when the phenomenon of separation occurs (the positive and negative outcomes are perfectly separated by a covariate and the estimate of a parameter consequently diverges to infinity without this correction). The odds ratios are expressed for a level of covariate compared to the reference level of the covariate. The tables also display 95% confidence intervals for these parameters along with their P values. <br><br>

The graph displays the odds ratios together with the 95% confidence intervals.<br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button.<br><br>

References:<br>
Heinze G, Schemper M (2002) A solution to the problem of separation in logistic regression. Statistics in medicine 21: 2409-2419."
    )})}

# RegressionOne_RCS ####
if (outputName=="RegressionOne_RCS") {
  description <- renderText({
    paste(
"The graphs represent modeling the association of the outcome variables with the chosen covariate using restricted cubic splines. Non linear associations might be apparent from the graphs. <br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button. <br><br>

References: <br>
Harrel FE, Lee KL, Pollock BG (1988) Regression models in clinical studies: determining relationships between predictors and response. Journal of the National Cancer Institute 80: 1198-1202."
    )})}


# RegressionAll ####
if (outputName=="RegressionAll") {
  description <- renderText({
    paste(

"Modelling is done for each outcome variable separately, but over all evaluation occasions while allowing the intercept to vary for each subject (using a mixed model). Tables display the estimated intercepts for the models and the estimated regression coefficients (either odds ratios for binary or beta coefficients for numerical and categorical variables). The graphs display the estimated coefficients along with their 95% confidence intervals.<br><br>

Choosing 'Outcome~Covariate+Subject(random effect)' will build models for studying the association of one covariate with the outcome, without adjusting the analysis for the other covariates. Choosing either 'Outcome~Covariate+Evaluation occasion+Subject(random effect)' or 'Outcome~Covariate+Time since inclusion+Subject(random effect)' will also evaluate time in the model. Evaluation occasions are modeled as categorical covariates while time since inclusion is modeled as a numerical covariate.<br><br>

You can copy the graph(s) by right clicking on them (selecting 'Copy Image' or 'Save Image as...') or download them as Postscript graphics by clicking the 'Download' button.

References:<br>
Gelman A, Hill J (2006) Data analysis using regression and multilevel/hierarchical models. Cambridge University Press."
    )})}

##### Return value ######
return(description)
}