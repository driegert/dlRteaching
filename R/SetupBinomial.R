SetupBinomial <- R6::R6Class("SetupBinom",
  inherit = NULL,

  ### NEEDS a bunch of work.
  # should create values for n, p, x, related to others such
  # that the probabilities aren't ridiculously low or high, but also
  # constrains possible x values as to be non-zero probability.

##########
# PUBLIC #
##########
  public = list(
    initialize = function(q_text, n, p, x, logical_op = NULL){
      private$.q_text <- q_text
      private$.n <- n
      private$.p <- p
      private$.x <- x

      if (!is.null(logical_op)){
        private$.logical_op <- logical_op
      }
    }
  ),

###########
# PRIVATE #
###########
  private = list(
    .q_text,
    .n,
    .p,
    .x,
    .logical_op = c("at least", "at most", "more than"
                     , "less than", "exactly")
  ),

##########
# ACTIVE #
##########
  active = list(

  )
)
