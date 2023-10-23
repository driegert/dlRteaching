# bb <- paste(readLines("~/Downloads/temp/webwork_qmd/test.qmd"), collapse = "\n")

ww_simp <- paste(readLines("tests/ww_simple.pg"), collapse = "\n")

sink("tests/ww_simple-out.pg")
cat(ww_simp)
sink()


# read in the file and store as a single long string
# qmd_text <- paste(readLines("tests/qmd_simple.qmd"), collapse = "\n")

# instead, use this approach to store each line as an element of a character vector
qmd_text <- paste(readLines("tests/qmd_simple.qmd"))

# pull out the yaml
## THIS WON'T WORK
# pattern <- "---\n(.*?)\n---"
#
# grep(pattern, qmd_text, perl = TRUE, value = TRUE)
# regexpr(pattern, qmd_text, perl = TRUE)


## Package testing

qmd <- QmdParser$new("tests/qmd_simple.qmd")
qmd$extract_yaml()


### First attempt, doesn't seem to work
pattern <- "---\n(.*?)\n---"

# Use regexpr to find the pattern in qmd_text
match <- regexpr(pattern, qmd_text, perl = TRUE)

# Extract matched content
yaml_block <- regmatches(qmd_text, match)

# If a match was found, extract only the YAML content
if (length(yaml_block) > 0 && nchar(yaml_block[[1]]) > 0) {
  yaml <- substring(yaml_block[[1]], 5, nchar(yaml_block[[1]]) - 4)
} else {
  yaml <- NULL
}


## Second attempt, using strplit instead
## ONLY IF FILE IS ONE ENTIRE STRING
split_content <- unlist(strsplit(qmd_text, "---"))
if (length(split_content) > 1) {
  yaml <- trimws(split_content[2])  # trimws is used to remove any leading or trailing whitespace
} else {
  yaml <- NULL
}


## USE read in VECTOR

qmd_text <- paste(readLines("tests/qmd_simple.qmd"))
yaml_start_stop <- grep("---", qmd_text, fixed = TRUE)
# --- can create horizontal rules which we want to leave in the files
# so, we only take the first two indices
yaml_idx <- (yaml_start_stop[1]+1):(yaml_start_stop[2]-1)
yaml <- qmd_text[yaml_idx]

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



vector_of_strings <- c("title: A Title: second title", "subtitle: Secondary goodness", "author: Dave Riegert: another info")

# Find the position of the first ": " in each string
first_colon <- sapply(vector_of_strings, function(x) {
  match.start <- regexpr(": ", x)[1]
  if (match.start == -1) return(character(0))  # If ": " not found, return empty string
  list(before = substr(x, 1, match.start-1), after = substr(x, match.start+2, nchar(x)))
})
