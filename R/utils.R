#' Cat Wrapper - One Newline
#' @description A wrapper for cat that adds a newline at the
#' end of the output and uses an empty separator.
#'
#' @export
ct <- function (..., file = "", sep = "", fill = FALSE, labels = NULL,
                append = FALSE)
{
  cat(..., "\n", file = file, sep = sep, fill = fill
      , labels = labels, append = append)
}

#' Cat Wrapper - Two Newlines
#' @description A wrapper for cat that adds a newline at the
#' end of the output and uses an empty separator.
#'
#' @export
ctt <- function (..., file = "", sep = "", fill = FALSE, labels = NULL,
                 append = FALSE)
{
  cat(..., "\n\n", file = file, sep = sep, fill = fill
      , labels = labels, append = append)
}
