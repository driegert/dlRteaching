Problem <- R6::R6Class("Problem",
  inherit = NULL,

##########
# PUBLIC #
##########
  public = list(
    initialize = function(title, author = ""
                          , date = Sys.Date()
                          , topic = "", subtopic = ""){
      private$.title <- title
      private$.author <- author
      private$.date <- date
      private$.topic <- topic
      private$.subtopic <- subtopic
    }
  ),

###########
# PRIVATE #
###########
  private = list(
    .author = "",
    .date = "",
    .marks = NA,
    .subtopic = "",
    .title = ""
  ),

##########
# ACTIVE #
##########
  active = list(

  )
)
