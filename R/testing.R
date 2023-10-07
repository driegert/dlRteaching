## Testing

test_function <- function(){
  ggbinom_pmf(n = 20, p = 0.7, x_shaded = 9:20)

  ggnorm_pdf(mean = 0, sd = 1, x_shaded = c(-1, 2), lower.tail = TRUE)


}
