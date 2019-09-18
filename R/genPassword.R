genPassword <- function() {
  set.seed(as.integer(Sys.time()))
  v1 <- sample(letters, 6, replace = TRUE)
  v2 <- sample(LETTERS, 6, replace = TRUE)
  v3 <- sample(0:9, 6, replace = TRUE)
  paste(sample(c(v1, v2, v3)), collapse = "")
}
