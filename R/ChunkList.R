ChunkList <- R6::R6Class(
  "ChunkList",

  ##########
  # PUBLIC #
  ##########
  public = list(
    initialize = function(n = 0){
      private$.n_chunk <- n
    },

    add_chunk = function(chunk_text){
      chunk <- Chunk$new(chunk_text)
      # private$.rchunk_list[[private$.n_chunk]] <- rchunk
      # private$.n_chunk <- private$.n_chunk + 1
    }
  ),

  ###########
  # PRIVATE #
  ###########
  private = list(
    .n_chunk = NA,
    .n_plt = 0,
    .n_r = 0,
    .n_ww = 0,
    .chunk_list = list(),
    # .chunk_idx = NA,
    .chunk_types = list()
  ),

  ##########
  # ACTIVE #
  ##########
  active = list(

  )
)
