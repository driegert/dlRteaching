ChunkList <- R6::R6Class(
  "ChunkList",

  ##########
  # PUBLIC #
  ##########
  public = list(
    initialize = function(){
      
    },

    add_chunk = function(chunk_text){
      chunk <- Chunk$new(chunk_text)
      
      if (chunk$name %in% private$.chunk_names || is.na(chunk$name)) {
        chunk$name <- private$.get_untitled()
      }
      
      private$.chunk_list[[chunk$name]] <- chunk
      private$.chunk_names <- c(private$.chunk_names, chunk$name)
      private$.chunk_types <- c(private$.chunk_types, chunk$type)
      
      switch(chunk$type,
             "img" = {
               private$.n_img <- private$.n_img + 1
             },
             "tbl" = {
               private$.n_tbl <- private$.n_tbl + 1
             },
             "plt" = {
               private$.n_plt <- private$.n_plt + 1
             },
             "r" = {
               private$.n_r <- private$.n_r + 1
             },
             "ww" = {
               private$.n_ww <- private$.n_ww + 1
             },
             {
               private$.n_unkown <- private$.n_unkown + 1
             }
      )
      private$.n_chunk <- private$.n_chunk + 1
    },
    
    print_r = function(){
      r_idx <- which(private$.chunk_types == "r")
      
      if (length(r_idx) == 0){
        return()
      }
      
      for (i in 1:length(r_idx)){
        private$.chunk_list[[private$.chunk_names[r_idx[i]]]]$print()
      }
    }
  ),

  ###########
  # PRIVATE #
  ###########
  private = list(
    .n_chunk = 0,
    .n_img = 0,
    .n_tbl = 0,
    .n_plt = 0,
    .n_r = 0,
    .n_ww = 0,
    .n_unkown = 0,
    .chunk_list = list(),
    .chunk_names = c(),
    # .chunk_idx = NA,
    .chunk_types = c(),
    .cur_untitled = 1,
    
    .get_untitled = function(){
      if (private$.cur_untitled < 10){
        num <- paste0("0", private$.cur_untitled)
      } else {
        num <- private$.cur_untitled
      }
      private$.cur_untitled <- private$.cur_untitled + 1
      
      paste0("untitled", num)
    }
  ),

  ##########
  # ACTIVE #
  ##########
  active = list(
    n = function(missing){
      if (missing){
        private$.n_chunk
      } else {
        private$.n_chunk <- value
      }
    },
    
    n_img = function(missing){
      if (missing){
        private$.n_img
      } else {
        stop("Set on creation.")
      }
    },
    
    n_tbl = function(missing){
      if (missing){
        private$.n_tbl
      } else {
        stop("Set on creation.")
      }
    },
    
    n_plt = function(missing){
      if (missing){
        private$.n_plt
      } else {
        stop("Set on creation.")
      }
    },
    
    n_r = function(missing){
      if (missing){
        private$.n_r
      } else {
        stop("Set on creation.")
      }
    },
    
    n_ww = function(missing){
      if (missing){
        private$.n_ww
      } else {
        stop("Set on creation.")
      }
    },
    
    n_unkown = function(missing){
      if (missing){
        private$.n_unkown
      } else {
        stop("Set on creation.")
      }
    },
    
    chunk_list = function(missing){
      if (missing){
        private$.chunk_list
      } else {
        stop("Set on creation.")
      }
    },
    
    chunk_names = function(missing){
      if (missing){
        private$.chunk_names
      } else {
        stop("Set on creation.")
      }
    },
    
    chunk_types = function(missing){
      if (missing){
        private$.chunk_types
      } else {
        stop("Set on creation.")
      }
    }
  )
)
