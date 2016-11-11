
gcd <- function(a, b) {
  c <- NULL
  if (a == b) {
    c <- a
  } else {
    while (a - ((a %/% b) * b) != 0) {
      c = (a - ((a %/% b) * b))
      a <- b
      b <- c
    }
  }
  return(c)
}

gcd(10, 10)

Reduce(gcd, vc)

