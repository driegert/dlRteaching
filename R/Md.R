Md <- R6::R6Class(
  "Md",

  ##########
  # PUBLIC #
  ##########
  public = list(
    initialize = function(text){
      private$.md_text <- text
    },
    
    print_ww = function(){
      
    }
  ),

  ###########
  # PRIVATE #
  ###########
  private = list(
    .md_text <- NA,
    
    .ans_num = function(output = c("ww", "qmd")){
      if (output[1] == "ww"){
        result <- sub("ANS_NUM\\{([^}]+)\\}", "\\1", private$.md_text)
        ### START HERE - how to deal with the answers!
      }
    },
    
    .ans_dd = function(output = c("ww", "qmd")){
      
    }
  ),

  ##########
  # ACTIVE #
  ##########
  active = list(

  )
)
