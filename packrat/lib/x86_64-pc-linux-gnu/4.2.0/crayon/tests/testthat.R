
if (Sys.getenv("NOT_CRAN") == "true") {
  library(crayon)
  library(testthat)
  test_check("crayon")
}
