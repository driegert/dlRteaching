#' @export
Chunk <- R6::R6Class(
  "Chunk",

  ##########
  # PUBLIC #
  ##########
  public = list(
    initialize = function(text){
      private$.text <- text
      self$parse_chunk()
    },

    parse_chunk = function(){
      prop_idx <- which(startsWith(private$.text, "#| "))
      if (length(prop_idx) == 0){
        private$.type <- "r"
        private$.label <- NA
      } else {
        # find all the properties (i.e., start with "#| ")
        tmp_text <- strsplit(private$.text[prop_idx], "#| ", fixed = TRUE) |>
          lapply(FUN = \(x) x[2]) |> unlist()

        # split the property type from the value
        properties <- tmp_text |> trimws() |> strsplit(": ", fixed = TRUE) |>
          (\(x) do.call("rbind", x))() |> as.data.frame()
        names(properties) <- c("property", "value")

        # in particular deal with the label property which identifies whether
        # the chunk contains R, WW, or other code to be dealt with specially
        tmp <- properties |>
          dplyr::filter(property == "label") |>
          dplyr::pull(value) |>
          strsplit(split = "-", fixed = TRUE) |>
          unlist()

        private$.type <- ifelse(tmp[1] %in% private$.chunk_types_allowed,
                                      tmp[1], "r")
        private$.label <- ifelse(length(tmp) == 2, tmp[2], tmp[1])

        private$.properties <- data.frame(property = c("label", "type")
          , value = c(private$.label
                      , private$.type))
        private$.properties <- rbind(private$.properties
          , properties[properties$property != "label", ])

        private$.name <- paste(private$.type, private$.label, sep = "-")
      }

      private$.code_idx <- (1:length(private$.text))[-prop_idx]
      private$.code <- private$.code_idx |>
        (\(x) private$.text[x])()
    },
    
    print = function(output = c("ww", "qmd"), ...){
      if (output[1] == "ww"){
        to_print <- switch(private$.type,
               r = paste("rserve_eval('{"
                         , paste(private$.code, collapse = "\n")
                         , "}');")
               # , ww = paste("```{ww ", private$.name, "}\n"
               #              , private$.code, "\n"
               #              , "```", sep = "")
               # , plt = paste("```{r ", private$.name, "}\n"
               #               , private$.code, "\n"
               #               , "```", sep = "")
               # , tbl = paste("```{r ", private$.name, "}\n"
               #               , private$.code, "\n"
               #               , "```", sep = "")
               # , img = paste("```{r ", private$.name, "}\n"
               #               , private$.code, "\n"
               #               , "```", sep = "")
               , stop("Chunk type not recognized.")
        )
        
        ctt(to_print)
      }
    }
  ),

  ###########
  # PRIVATE #
  ###########
  private = list(
    .name = NA,
    .text = NA,
    .type = NA,
    .label = NA,
    .properties = NA,
    .chunk_types_allowed = c("r", "ww", "plt", "tbl", "img"),
    .code_idx = NA,
    .code = NA
  ),

  ##########
  # ACTIVE #
  ##########
  active = list(
    name = function(value){
      if (missing(value)){
        return(private$.name)
      } else {
        private$.name <- value
      }
    },

    text = function(value){
      if (missing(value)){
        return(private$.text)
      } else {
        stop("Code defined in the chunk.")
      }
    },

    type = function(value){
      if (missing(value)){
        return(private$.type)
      } else {
        stop("Type defined in the chunk label.")
      }
    },

    label = function(value){
      if (missing(value)){
        return(private$.label)
      } else {
        private$.label <- value
      }
    },

    properties = function(value){
      if (missing(value)){
        return(private$.properties)
      } else {
        stop("Properties defined in the chunk label.")
      }
    }
  )
)
