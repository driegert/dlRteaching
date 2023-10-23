
#' @export
Yaml <- R6::R6Class(
  "Yaml",

##########
# PUBLIC #
##########
  public = list(
    initialize = function(text){
      self$parse_yaml(text)
      # --- can create horizontal rules which we want to leave in the files
      # so, we only take the first two indices
      invisible(self)
    },

    parse_yaml = function(yaml){
      properties.tmp <- lapply(yaml, \(xx) {
        tmp <- unlist(strsplit(x = xx, split = ": ", fixed = TRUE))
        if (length(tmp) > 2){
          c(tmp[1], paste(tmp[2:length(tmp)], collapse = ": "))
        } else {
          tmp
        }
      })

      yaml_properties <- as.data.frame(do.call("rbind", properties.tmp))
      names(yaml_properties) <- c("property", "value")

      # private$.yaml_stend_idx <- yaml_start_stop
      private$.yaml <- yaml
      # private$.yaml_idx <- yaml_idx
      private$.yaml_properties <- yaml_properties
    },

    print = function(..., output = c("ww", "qmd")){
      if (output[1] == "ww"){
        # ct(private$.yaml_to_ww())
      } else if (output[1] == "qmd") {
        cat("---\n", private$.yaml, "---\n")
      } else {
        cat(private$.yaml)
      }
    },
    
    ww_top = function(){
      'DOCUMENT();
      
      # Macros help files:
      # https://webwork.maa.org/pod/pg/macros/
      loadMacros(
        "PGstandard.pl",
        "MathObjects.pl",
        "PGchoicemacros.pl",
        "PGgraders.pl",
        "parserRadioButtons.pl",
        "PGML.pl",
        "PGcourse.pl",
        "RserveClient.pl",
        "parserPopUp.pl",
        "niceTables.pl",
        "parserOneOf.pl"
      );
      # Print problem number and point value (weight) for the problem
      TEXT(beginproblem());
      
      $tol = 0.01;
      $tolType = "relative"; # or "absolute"
      
      #  Setup
      Context("Numeric");
      Context()->flags->set(tolerance => $tol, tolType => $tolType);\n\n'
    },
    
    ww_bottom = function(partialCorrect = TRUE){
      paste0('END_PGML
      
      #  Answers
      # ANS(Real($mean)->cmp);
      
      $showPartialCorrectAnswers = ', as.numeric(partialCorrect), ';
      
      ENDDOCUMENT();')
    }
  ),

###########
# PRIVATE #
###########
  private = list(
    .yaml = "",
    # .yaml_idx = NA,
    .yaml_properties = NA,
    # .yaml_stend_idx = NA
    
    .yaml_to_ww = function(){
      ""
    }
  ),

##########
# ACTIVE #
##########
  active = list(
    yaml = function(value){
      if(missing(value)){
        private$.yaml
      } else {
        stop("Initialize a new object.")
      }
    },

    yaml_idx = function(value){
      if(missing(value)){
        private$.yaml_idx
      } else {
        stop("Initialize a new object.")
      }
    },

    yaml_properties = function(value){
      if(missing(value)){
        private$.yaml_properties
      } else {
        stop("Initialize a new object.")
      }
    },

    yaml_stend_idx = function(value){
      if(missing(value)){
        private$.yaml_stend_idx
      } else {
        stop("Initialize a new object.")
      }
    }
  )
)
