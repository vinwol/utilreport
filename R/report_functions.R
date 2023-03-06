#' Function to create a section header.
#'
#' @param header section header text.
#' @export
create_section_header <- function(header) {
  cat('\n <br> \n')
  cat('\n## ',header)
  #cat('\n <br> \n')
}

#' Function to create a subsection header.
#'
#' @param section_type type for the subsection. The subsection header can be included
#' and separated from the ype by a semicolon.
#' @export
create_subsection_header <- function(section_type) {
  if (section_type != 'section') {
    subsection_header <- base::strsplit(section_type, ':', fixed = TRUE)[[1]][2]
    #cat('\n <br> \n')
    cat('\n### ',subsection_header)
  }
}

#' Function to create a header for a tabbed section.
#'
#' @param header tabbed section header text.
#' @export
create_header_for_tabbing <- function(header) {
  #cat('\n <br> \n')
  cat('\n## ',header,' {.tabset}')
}

#' Function to create a description.
#'
#' @param description description text.
#' @param tabbing_status information for the tabbing (untabbed or tabbed).
#' @export
create_description <- function(description, tabbing_status) {
  if (tabbing_status != 'tabbed') {
    #cat('\n', '<font size="3"><b>',description,'</b></font><br>','\n')
    cat('\n', '<font size="3"><b>',description,'</b></font>','\n')
  } else {
    cat('\n <br> \n')
    cat('\n### ',description)
    #cat('\n <br> \n')
  }
}

#' Function to create a content.
#'
#' @param content content for the report.
#' @param type data type of the content.
#' @export
create_content <- function(content, type) {
  if ('character' %in% type) {
    cat('\n')
    cat(content)
    cat('\n')
  } else if ('data.frame' %in% type ||
             'data.table' %in% type ||
             'matrix' %in% type ||
             'tbl_df' %in% type ||
             tibble::is_tibble(type)) {
    cat('\n','<div style="width:100%; margin-top:0px; margin-bottom:-220px;">','\n')
    print(content)
    cat('\n','</div>','\n')
  } else if ('gg' %in% type ||
             'ggplot' %in% type ||
             ggplot2::is.ggplot(content)) {
    cat('\n')
    print(content)
    cat('\n')
  } else if ('htmlwidget' %in% type) {
    print(content)
    cat('\n')
  } else {
    cat('\n','Could not identify the data type: ',type,'\n')
  }
}

#' Function to create the whole report document.
#'
#' @param section_list section list.
#' @param section_type_list section type list.
#' @param tabbing_list tabbing list.
#' @param description_list description list.
#' @param content_list content list.
#' @param type_list content type list.
#' @export
create_report <- function(section_list,
                          section_type_list,
                          tabbing_list,
                          description_list,
                          content_list,
                          type_list) {
  # Check length.
  l <- length(content_list)
  if (length(section_list) != l ||
      length(section_type_list) != l ||
      length(tabbing_list) != l ||
      length(description_list) != l) {
    stop('Length of lists not identical!')
  }
  section_status <- 'NULL'
  tabbing_status <- 'NULL'
  for (index in 1:length(description_list)) {
    section <- section_list[[index]]
    section_type <- section_type_list[[index]]
    tabbing <- tabbing_list[[index]]
    description <- description_list[[index]]
    content <- content_list[[index]]
    type <- type_list[[index]]
    if (section != section_status && tabbing != 'tabbed') {
      # We have a new section and no tabbing.
      create_section_header(section)
      section_status <- section
      tabbing_status <- tabbing
    }
    if (section != section_status && tabbing == 'tabbed') {
      # We have a new section with tabbing.
      create_header_for_tabbing(section)
      section_status <- section
      tabbing_status <- tabbing
    }
    create_subsection_header(section_type)
    create_description(description, tabbing_status)
    create_content(content, type)
  }
}



