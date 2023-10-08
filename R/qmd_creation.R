
#' @export
qmd_yaml <- function(title, author = "Dave Riegert"
                     , date = "today", date_format = "long"
                     , format = "pdf"){

  date <- ifelse(date == "today", "today", paste0("\"", date, "\""))
  yaml <- paste0("---\n"
                 , "title: \"", title, "\"\n"
                 , "author: \"", author, "\"\n"
                 , "date: ", date, "\n"
                 , "date-format: ", date_format, "\n"
                 , "format: ", format, "\n"
                 , "---\n\n")

  yaml
}

