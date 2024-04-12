# functions for simulation

# general replication function
sim <- function(f, n = 100, k = 1000, block = FALSE) {
  replicate(n = k, expr = f(n = n, block = block))
}

# simplest DAG X -> Y
base <- function(n = 100, block = NULL) {
  x <- rnorm(n)
  y <- x + rnorm(n)
  m <- lm(y ~ x)
  m$coefficients[["x"]]
}

# chain X -> (A) -> Y
chain <- function(n = 100, block = FALSE) {
  x <- rnorm(n)
  a <- x + rnorm(n)
  y <- a + rnorm(n)
  if (block) m <- lm(y ~ x + a) else m <- lm(y ~ x)
  m$coefficients[["x"]]
}

# confounding (A) -> X -> Y <- (A)
conf <- function(n = 100, block = FALSE) {
  a <- rnorm(n)
  x <- a + rnorm(n)
  y <- a + x + rnorm(n)
  if(block) m <- lm(y ~ x + a) else m <- lm(y ~ x)
  m$coefficients[["x"]]
}

# collider (B) <- X -> Y -> (B)
coll <- function(n = 100, block = FALSE) {
  x <- rnorm(n)
  y <- x + rnorm(n)
  b <- x + y + rnorm(n)
  if (block) m <- lm(y ~ x + b) else m <- lm(y ~ x)
  m$coefficients[["x"]]
}
