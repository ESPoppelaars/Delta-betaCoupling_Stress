# Custom function to check the magnitude of Cohen's d values
cohen.d.magnitude <- function(x) {
  if (x < 0.2) {
    y <- "negligible"
  } else if (x < 0.5) {
    y <- "small"
  } else if (x < 0.8) {
    y <- "medium"
  } else {
    y <- "large"
  } 
}