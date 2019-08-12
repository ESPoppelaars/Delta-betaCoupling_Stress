# Custom function to check the significance of p.values
p.value.sig <- function(x) {
  if (x < 0.001) {
    y <- "***"
  } else if (x < 0.01) {
    y <- "**"
  } else if (x < 0.05) {
    y <- "*"
  } else if (x < 0.1) {
    y <- "+"
  } else {
    y <- "not significant"
  } 
}