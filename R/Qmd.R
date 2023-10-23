#' @details
#' Portions written with the help of GPT4; in particular
#' the regex aspects.
#' @details
#' I would really like to have some kind of ID keeping track of ..
#' Indices that match the original file.
#'
#' @export
Qmd <- R6::R6Class(
  "Qmd",

##########
# PUBLIC #
##########
  public = list(
    initialize = function(file = NULL, text = NULL){
      private$.qmd_text <- paste(readLines("tests/qmd_simple.qmd"))
      names(private$.qmd_text) <- 1:length(private$.qmd_text)
      private$.qmd_text_wrk <- private$.qmd_text

      self$find_yaml()
      self$find_chunks()
    },

    find_chunks = function(){
      # GPT4 suggestion
      start_indices <- which(startsWith(private$.qmd_text, "```{r"))
      end_indices <- integer(0)

      if (length(start_indices) == 0){
        private$.Rchunks <- 0
        return()
      }

      for (start in start_indices) {
        end_chunk <- which(startsWith(private$.qmd_text[(start+1):length(private$.qmd_text)], "```"))[1] + start
        end_indices <- c(end_indices, end_chunk)
      }

      if (length(start_indices) != length(end_indices)){
        stop("Number of start and end indices do not match for finding RChunks.")
      }

      private$.chunk_stend_idx <- list(start = start_indices, end = end_indices)
      private$.chunk_idx <- list(start = start_indices+1, end = end_indices-1)
      private$.n_chunk <- length(start_indices)

      private$.chunks <- ChunkList$new(n = private$.n_chunk)
      for (i in 1:private$.n_chunk){
        private$.chunks$add_chunk(
          private$.qmd_text[private$.chunk_idx$start[i]:private$.chunk_idx$end[i]]
        )
      }
    },

    #' @details
    #' could be more than 2 "---" (horizontal rule) and so take
    #' the first two.
    find_yaml = function(){
      yaml_start_stop <- grep("---", private$.qmd_text, fixed = TRUE)

      yaml_idx <- (yaml_start_stop[1]+1):(yaml_start_stop[2]-1)
      private$.yaml_stend_idx <- yaml_start_stop
      private$.yaml <- Yaml$new(private$.qmd_text[yaml_idx])
    }

    # modify_wrk = function(idx, remove = TRUE){
    #   if (remove){
    #     private$.qmd_text_rmv_idx <- c(private$.qmd_text_rmv_idx, idx)
    #   } else {
    #     private$.qmd_text_rmv_idx <- private$.qmd_text_rmv_idx[private$.qmd_text_rmv_idx != idx]
    #   }
    #
    #   private$.qmd_txt_work <- private$.qmd_text[-private$.qmd_text_rmv_idx]
    # }
  ),

###########
# PRIVATE #
###########
  private = list(
    .chunks = NA,
    .chunk_idx = NA, # actual code
    .chunk_stend_idx = NA, # includes "```{r" and "```"
    .md_text = NA,
    .n_chunk = NA,
    .qmd_text = "",
    .qmd_text_rmv_idx = c(),
    .qmd_text_wrk = "",
    .yaml = NA,
    .yaml_stend_idx = NA
  ),

##########
# ACTIVE #
##########
  active = list(
    n_rchunks = function(value){
      if (missing(value)){
        private$.n_rchunks
      } else {
        stop("Set automatically.")
      }
    },

    qmd_text = function(value){
      if(missing(value)){
        private$.qmd_text
      } else {
        private$.qmd_text <- value
      }
    },

    yaml = function(value){
      if(missing(value)){
        private$.yaml
      } else {
        stop("Initialize a new object.")
      }
    },

    yaml_stend_idx = function(value){
      if(missing(value)){
        private$.yaml$yaml_stend_idx
      } else {
        private$.yaml_stend_idx <- value
      }
    }
  )
)
