# utilreport

R package for easy R Markdown HTML reporting which is decoupled from the analysis code.

Installation: 
<br>
devtools::install_github("https://github.com/vinwol/utilreport")

# Overview

The goal of `utilreport` is to decouple the reporting code from the analysis code so that one can concentrate on analysing a certain data set.
Every piece of information (e.g. a plot or a data frame) which needs to be presented in the HTML report is added to the reporter object during the analysis by one method call. When the analysis is finished, the reporter object then creates the HTML report from a generic R Markdown template.

## Example of an analysis with reporting

The following shows an example analysis using the `utilreport` package.
First, load the required R packages and create an instance of the Reporter class providing author and title 
to be shown in the HTML report.  

```{r message = FALSE, eval = FALSE}
library(ggplot2)
library(ggcorrplot)
library(heatmaply)
library(utilreport)
reporter <- Reporter$new('Data Scientist','Example')
```

Next, we add short introduction to start off the report.
For this we use the method `add_to_report()` with parameters for the section header, the section type, if it is tabbed or untabbed, and some text.  

```{r eval = FALSE}
reporter$add_to_report('Introduction',
                       'section',
                       'untabbed',
                       'This analysis is meant as an example.')
```

In the next step, we add the data set which is being used for the example analysis here to the reporter object.

```{r eval = FALSE}
mtcars <- mtcars
reporter$add_to_report('Data Description',
                       'section',
                       'untabbed',
                       'The data used for the analysis', 
                       mtcars)
```

Now we create a scatter plot and a box plot using ggplot and add it to the reporter object as tabbed content.

```{r eval = FALSE}
scatter_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point()
reporter$add_to_report('Data Exploration',
                       'section',
                       'tabbed',
                       'Scatterplot', 
                       scatter_plot)
box_plot <- ggplot(mtcars,aes(x = factor(am), y = mpg)) + 
  geom_boxplot() +
  xlab("Transmission Type") + 
  ylab("Miles Per Gallon (mpg)") 
reporter$add_to_report('Data Exploration',
                       'section',
                       'tabbed',
                       'Boxplot', 
                       box_plot)
```

In the next step, we add a section with subsections for the correlation analysis to the reporter object.

```{r eval = FALSE}
mat <- cor(mtcars, use = "pairwise.complete.obs")
reporter$add_to_report('Correlation Analysis',
                       'subsection:Part-1',
                       'untabbed',
                       'Data of correlation matrix', 
                       mat)
correlation_matrix_plot <- ggcorrplot::ggcorrplot(mat)
reporter$add_to_report('Correlation Analysis',
                       'subsection:Part-2',
                       'untabbed',
                       'Plot of correlation matrix', 
                       correlation_matrix_plot)
```

The following shows an example how to add an HTML widget to the reporter object.

```{r eval = FALSE}
correlation_matrix_interactive_plot <- heatmaply::heatmaply(mat, 
                                                            scale = 'none')
reporter$add_to_report('Correlation Analysis', 
                       'subsection:Part-3',
                       'untabbed',
                       'Interactive plot of correlation matrix', 
                       correlation_matrix_interactive_plot)
```

At the end of the analysis, the HTML report can be created by the method `render_report()` 
providing the file name and directory as parameters.

```{r eval = FALSE}
reporter$render_report('analysis_report.html', '.')
```

Another useful report can be generated by the method `render_environment_and_setup_report()` which 
shows the R version and R packages that are used and if provided the parameters used for the different functions in the analysis.

```{r eval = FALSE}
reporter$render_environment_and_setup_report('env_report.html', '.')
```

Another useful feature is to create a master report which provides the links to all the other reports.
It can be created with the method `render_master_report()` which takes as parameters the file name, the list of HTML report files and the list 
of report names to show in the master report.

```{r eval = FALSE}
report_files <- list('env_report.html','analysis_report.html')
report_names <- list('Setup Report','Analysis Report')
reporter$render_master_report('master_report.html', report_files, report_names)
```





