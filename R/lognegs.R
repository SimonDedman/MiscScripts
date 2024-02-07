# lognegs. Simon Dedman simondedman@gmail.com 2023-05-23
# log positive & negatives and retain the directionality
lognegs <- function(x) {
  posindex <- which(x > 0)
  negindex <- which(x < 0)
  x[posindex] <- log(x[posindex])
  x[negindex] <- -log(-x[negindex])
  return(x)
}
