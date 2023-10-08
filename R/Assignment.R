# My thought here is to have assignments consist of
# a list of problem sets, where a random problem is
# selected from each set.
# The problem sets would all be of a similar
# difficulty, but different problems.

#' @export
Assignment <- R6::R6Class("Assignment",
  inherit = NULL,

  ##########
  # PUBLIC #
  ##########
  public = list(

  ),

  ###########
  # PRIVATE #
  ###########
  private = list(
    .due_date = "",
    .n_questions = NA,
    .problems = list(),
    .total_marks = NA
  ),

  ##########
  # ACTIVE #
  ##########
  active = list(

  )
)
