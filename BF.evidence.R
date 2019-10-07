# Custom function to calculate Bayes factor interpretation (based on Kass & Raftery, 1995)
BF.evidence <- function(x) {
  if (x <= 1/100) {
    y <- "Decisive evidence for H0 **"
  } else if (x <= 1/10) {
    y <- "Strong evidence for H0 **"
  } else if (x <= 1/3.2) {
    y <- "Substantial evidence for H0 *"
  } else if (x < 1) {
    y <- "Barely-worth-mentioning evidence for H0 +"
  } else if (x >= 100) {
    y <- "Decisive evidence for H1 **"
  } else if (x >= 10) {
    y <- "Strong evidence for H1 **"
  } else if (x >= 3.2) {
    y <- "Substantial evidence for H1 *"
  } else if (x > 1) {
    y <- "Barely-worth-mentioning evidence for H1 +"
  } else {
    y <- "Inconclusive"
  } 
}
