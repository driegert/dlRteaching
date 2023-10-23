#' @export
Chunk <- R6::R6Class(
  "Chunk",

  ##########
  # PUBLIC #
  ##########
  public = list(
    initialize = function(text){
      private$.chunk_text <- text
      self$parse_chunk()
    },

    parse_chunk = function(){
      prop_idx <- which(startsWith(private$.chunk_text, "#| "))
      if (length(prop_idx) == 0){
        private$.chunk_type <- "r"
        private$.chunk_label <- NA
      } else {
        browser()
        # find all the properties (i.e., start with "#| ")
        tmp_text <- strsplit(private$.chunk_text[prop_idx], "#| ", fixed = TRUE) |>
          lapply(FUN = \(x) x[2]) |> unlist()

        # split the property type from the value
        chunk_properties <- tmp_text |> trimws() |> strsplit(": ", fixed = TRUE) |>
          (\(x) do.call("rbind", x))() |> as.data.frame()
        names(chunk_properties) <- c("property", "value")

        # in particular deal with the label property which identifies whether
        # the chunk contains R, WW, or other code to be dealt with specially
        tmp <- chunk_properties |>
          dplyr::filter(property == "label") |>
          dplyr::pull(value) |>
          strsplit(split = "-", fixed = TRUE) |>
          unlist()

        private$.chunk_type <- ifelse(tmp[1] %in% private$.chunk_types_allowed,
                                      tmp[1], "r")
        private$.chunk_label <- ifelse(length(tmp) == 2, tmp[2], tmp[1])

        private$.chunk_properties <- data.frame(property = c("label", "type")
          , value = c(private$.chunk_label
                      , private$.chunk_type))
        private$.chunk_properties <- rbind(private$.chunk_properties
          , chunk_properties[chunk_properties$property != "label", ])

        private$.chunk_name <- paste(private$.chunk_type, private$.chunk_label, sep = "-")
      }

      private$.code_idx <- (1:length(private$.chunk_text))[-prop_idx]
      private$.code <- private$.code_idx |>
        (\(x) private$.chunk_text[x])()
    }
  ),

  ###########
  # PRIVATE #
  ###########
  private = list(
    .chunk_name = NA,
    .chunk_text = NA,
    .chunk_type = NA,
    .chunk_label = NA,
    .chunk_properties = NA,
    .chunk_types_allowed = c("r", "ww", "plt", "tbl", "img"),
    .code_idx = NA,
    .code = NA
  ),

  ##########
  # ACTIVE #
  ##########
  active = list(
    chunk_name = function(value){
      if (missing(value)){
        return(private$.chunk_name)
      } else {
        private$.chunk_name <- value
      }
    },

    chunk_text = function(value){
      if (missing(value)){
        return(private$.chunk_text)
      } else {
        stop("Code defined in the chunk.")
      }
    },

    chunk_type = function(value){
      if (missing(value)){
        return(private$.chunk_type)
      } else {
        stop("Type defined in the chunk label.")
      }
    },

    chunk_label = function(value){
      if (missing(value)){
        return(private$.chunk_label)
      } else {
        private$.chunk_label <- value
      }
    },

    chunk_properties = function(value){
      if (missing(value)){
        return(private$.chunk_properties)
      }
      } else {
        stop("Properties defined in the chunk label.")
      }
    }
  )
)
