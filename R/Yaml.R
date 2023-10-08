#' @export
Yaml <- R6::R6Class("Yaml",
  inherit = NULL,

##########
# PUBLIC #
##########
  public = list(
    initialize = function(title, subtitle = ""
                          , author = "Dave Riegert"
                          , date = "today", date_format = "long"
                          , format = "pdf") {
      private$.title <- title
      private$.subtitle <- subtitle
      private$.author <- author
      private$.date <- date
      private$.date_format <- date_format
      private$.format <- format
    },

    get_yaml = function(n_newlines = 2){
      private$.check_date()
      private$.check_date_format()
      paste0("---\n"
             , "title: \"", private$.title, "\"\n"
             , "subtitle: \"", private$.subtitle, "\"\n"
             , "author: \"", private$.author, "\"\n"
             , "date: ", private$.date, "\n"
             , "date-format: ", private$.date_format, "\n"
             , "format: ", private$.format, "\n"
             , "---"
             , paste(rep("\n", n_newlines), collapse = ""))
    },

    print = function(...){
      private$.check_date()
      private$.check_date_format()
      self$get_yaml()
    }
  ),

###########
# PRIVATE #
###########
  private = list(
    .title = "",
    .author = "",
    .date = "",
    .date_format = "long",
    .format = "pdf",
    .allowed_date_formats = c("full", "long", "medium"
                              , "short", "iso"),
    .subtitle = "",

    .check_date = function(){
      private$.date <- ifelse(private$.date == "today", "today"
                     , paste0("\"", private$.date, "\""))
    },

    .check_date_format = function(){
      private$.date_format <- ifelse(
        private$.date_format %in% private$.allowed_date_formats
        , private$.date_format, paste0("\"", private$.date_format, "\""))
    }
  ),

##########
# ACTIVE #
##########
  active = list(
    set_code_wrap = function(value = TRUE){
      if (missing(value)){
        stop("Value must be specified")
      } else {
        if (! is.logical(value)){
          stop("Value must be TRUE or FALSE")
        }
        private$.code_wrap <- value
      }
      warning("This doesn't do anything yet.")
      # pdf:
      #   code-line-numbers: true
      # include-in-header:
      #  \usepackage{fancyvrb}
      #  \usepackage{fvextra}
      #  \DefineVerbatimEnvironment{Highlighting}{Verbatim}{breaklines,commandchars=\\\{\}}
    },

    set_date_format = function(value){
      if (missing(value)){
        stop("Value must be specified")
      } else {
        if (! value %in% private$.allowed_date_formats){
          warning(paste0("Be sure the date format is correct.
                  If using a style, the following are valid:"
            , paste(private$.allowed_date_formats, collapse = ", ")))
        }
        private$.date_format <- value
      }
    },

    set_format = function(value){
      if (missing(value)){
        stop("Value must be specified")
      } else {
        if (! value %in% c("pdf", "html", "docx", "odt", "epub")){
          stop("Value must be one of: pdf, html, docx, odt, epub.")
        }
        private$.format <- value
      }
    }
  ),
)
