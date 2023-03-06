#' Reporter class which collects information to be displayed in the report.
#'
#' @import ggplot2
#' @import tibble
#' @import dplyr
#' @import DT
#' @import htmltools
#' @import rmarkdown
#' @export
Reporter <- R6::R6Class(
  classname = "Reporter",
  public = list(
    #' @description
    #' Constructor to setup an Reporter class instance.
    #' @param author author to be shown in the report.
    #' @param title title to be shown in the report.
    initialize = function(author = '',
                          title = '') {
      private$author <- author
      private$title <- title
    },
    #' @description
    #' Method to add the parameters.
    #' @param parameter_list parameter list.
    add_parameter_list = function(parameter_list) {
      private$parameter_list <- parameter_list
      invisible(self)
    },
    #' @description
    #' Method to retrieve the parameters.
    get_parameter_list = function() {
      return(private$parameter_list)
    },
    #' @description
    #' Method for setting the dimensions of an HTML widget.
    #' @param width width of HTML widget.
    #' @param height height of HTML widget.
    set_htmlwidget_dimension = function(width, height) {
      private$htmlwidget_width <- width
      private$htmlwidget_height <- height
    },
    #' @description
    #' Method for configuring DT datatable to show CSV button or not.
    #' @param boolean_value TRUE or FALSE to switch showing CSV button.
    show_datatable_csv_button = function(boolean_value) {
      private$flag_csv_button <- bool_val
    },
    #' @description
    #' Method to add a certain piece of information to be displayed in the report.
    #' @param section section information.
    #' @param section_type type of section.
    #' @param tabbing tabbing information.
    #' @param description description information.
    #' @param content content information.
    add_to_report = function(section = '',
                             section_type = 'section',
                             tabbing = 'untabbed',
                             description = '',
                             content = '') {
      # Do some preparation here depending on the data type of the content.
      type <- class(content)
      if ('data.frame' %in% type ||
          'data.table' %in% type ||
          'matrix' %in% type ||
          'tbl_df' %in% type ||
          tibble::is_tibble(type)) {
        table_height <- 800
        if (nrow(content) < 10 && nrow(content) >= 5) {
          table_height <- 600
        }
        if (nrow(content) < 5) {
          table_height <- 500
        }
        # DT::datatable: options: dom:
        # B - Buttons
        # l - Length changing input control
        # f - Filtering input
        # r - pRocessing display element
        # t - Table
        # i - Table information summary
        # p - Pagination control
        if (private$flag_csv_button) {
          content <- htmltools::tagList(DT::datatable(data = content,
                                                      width = 1000,
                                                      height = table_height,
                                                      extensions = 'Buttons',
                                                      options = list(scrollX = TRUE,
                                                                     scrollY = '400px',
                                                                     scrollCollapse = TRUE,
                                                                     dom = 'Blfrtip',
                                                                     buttons = c('csv'))))
        } else {
          content <- htmltools::tagList(DT::datatable(data = content,
                                                      width = 1000,
                                                      height = table_height,
                                                      options = list(scrollX = TRUE,
                                                                     scrollY = '400px',
                                                                     scrollCollapse = TRUE,
                                                                     dom = 'Blfrtip')))
        }
      }
      if ('htmlwidget' %in% type) {
        cont <- htmltools::tagList(content)
        cont[[1]]$width <- private$htmlwidget_width
        cont[[1]]$height <- private$htmlwidget_height
        #formatted_content <- paste0('\n',cont,'\n')
        content <- cont
      }
      private$section_list[[length(private$section_list)+1]] <- section
      private$section_type_list[[length(private$section_type_list)+1]] <- section_type
      private$tabbing_list[[length(private$tabbing_list)+1]] <- tabbing
      private$description_list[[length(private$description_list)+1]] <- description
      private$content_list[[length(private$content_list)+1]] <- content
      private$type_list[[length(private$type_list)+1]] <- type
      invisible(self)
    },
    #' @description
    #' Convenient method to add an untabbed piece of information to be displayed in the report.
    #' The section header needs to be set by the method set_section().
    #' @param description description information.
    #' @param content content information.
    add_item = function(description = '',
                        content = '') {
      self$add_to_reportn(section = private$section,
                          section_type = 'section',
                          tabbing = 'untabbed',
                          description = description,
                          content = content)
      invisible(self)
    },
    #' @description
    #' Method which renders the report to HTML.
    #' @param directory directory name for the generated report.
    #' @param file file name for the generated report.
    render_report = function(file = '',
                             directory = '') {
      template <- 'report_template.Rmd'
      rmarkdown::render(input = system.file(template, package = 'utilreport'),
                        #output_format = rmarkdown::html_document(), # This causes issues with the sidemenu and is not needed.
                        output_file = file,
                        output_dir = directory,
                        params = list(report_title = private$title,
                                      report_author = private$author,
                                      reporter = self),
                        envir = parent.frame())
    },
    #' @description
    #' Method which renders the report about the R environment, methods and parameters used to HTML.
    #' @param directory directory name for the generated report.
    #' @param file file name for the generated report.
    render_environment_and_setup_report = function(file = '',
                                                  directory = '') {
      template <- 'report_environment.Rmd'
      rmarkdown::render(input = system.file(template, package = 'utilreport'),
                        output_format = rmarkdown::html_document(),
                        output_file = file,
                        output_dir = directory,
                        params = list(report_title = private$title,
                                      report_author = private$author,
                                      reporter = self),
                        envir = parent.frame())
    },
    #' @description
    #' Method which renders the HTML master report file which points to all the generated reports.
    #' @param file file name for the generated report.
    #' @param report_files HTML report files to include.
    #' @param report_names names of the included HTML report files.
    render_master_report = function(file = '',
                                    report_files = NULL,
                                    report_names = NULL) {
      html_header_str <- '
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="UTF-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Report</title>
        <style>
        h1 {margin:0px;padding:0px;}
        h2 {margin:0px;padding-bottom:5px;}
        h3 {margin:0px;padding-bottom:10px;}
        li {padding-bottom:5px;}
        .container {margin-left: 10px;}
        </style>
      </head>
      <div class="container">
      '
      html_title_str <- paste0('<h2>',private$title,'</h2>')
      html_author_str <- paste0('<h3>',private$author,'</h3>')
      html_date_str <- paste0('<h3>',format(Sys.time(), '%d %B, %Y'),'</h3>')
      html_tail_str <- '</div></body></html>'
      html_report_list <- c('<ul>')
      for (index in 1:length(report_files)) {
        report_file <- report_files[[index]]
        report_name <- report_names[[index]]
        elem_str <- paste0('<li><a href=',report_file,' target="_blank">',report_name,'</a></li>')
        html_report_list <- append(html_report_list, elem_str)
      }
      html_report_list <- append(html_report_list,'</ul>')
      html_reports_str <- paste(html_report_list, collapse = '')
      html_master_report_str <- paste0(html_header_str,
                                       html_title_str,
                                       html_author_str,
                                       html_date_str,
                                       html_reports_str,
                                       html_tail_str)
      write.table(html_master_report_str,
                  file = file,
                  quote = FALSE,
                  col.names = FALSE,
                  row.names = FALSE)
    },
    #' @description
    #' Method to set the current section header.
    #' @param section the section header.
    set_section = function(section) {
      private$section = section
      invisible(self)
    },
    #' @description
    #' Method to get the current section header.
    get_section = function() {
      return(private$section)
    },
    #' @description
    #' Method to retrieve the list storing the section information.
    get_section_list = function() {
      return(private$section_list)
    },
    #' @description
    #' Method to retrieve the list storing the section type information.
    get_section_type_list = function() {
      return(private$section_type_list)
    },
    #' @description
    #' Method to retrieve the list storing the tabbing information.
    get_tabbing_list = function() {
      return(private$tabbing_list)
    },
    #' @description
    #' Method to retrieve the list storing the description information.
    get_description_list = function() {
      return(private$description_list)
    },
    #' @description
    #' Method to retrieve the list storing the content information.
    get_content_list = function() {
      return(private$content_list)
    },
    #' @description
    #' Method to retrieve the list storing the content type information.
    get_type_list = function() {
      return(private$type_list)
    }
  ),
  private = list(
    author = '',
    title = '',
    template = '',
    directory = '',
    file = '',
    section = '',
    htmlwidget_width = 1200,
    htmlwidget_height = 1200,
    flag_csv_button = TRUE,
    parameter_list = list(),
    section_list = list(),
    section_type_list = list(),
    tabbing_list = list(),
    description_list = list(),
    content_list = list(),
    type_list = list()
  )
)
