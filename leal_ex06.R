NDD <- function(x, y, order) {
  m = matrix(nrow = (order + 1), ncol = (order + 2))
  m[, 1] = x
  m[, 2] = y
  
  
  for (i in 1:order) {
    for (j in 1:(order - i + 1)) {
      m[j, (i + 2)] = (m[(j + 1), (i + 1)] - m[(j), (i + 1)]) / (m[(i + j), 1] - m[(j), 1])
    }
  }
  eq_string = StringInterpolatingPolynomial(m, order)
  
  interpolating_polynomial = eval(parse(text = eq_string))
  return (list(func = interpolating_polynomial, m = m))
}

StringInterpolatingPolynomial <- function(m, order) {
  first_row = m[1,]
  first_col = m[, 1]
  eq_string = paste("f <- function (x) ", first_row[2], sep = " ", "+")
  
  for (i in 3:(order + 2)) {
    for (j in 1:(i - 2)) {
      factors = paste(sep = " ", "( x -", first_col[j], ")*")
      eq_string = paste(sep = " ", eq_string, factors)
    }
    eq_string = paste(sep = " ", eq_string, first_row[i])
    if (i != (order + 2))
      eq_string = paste(sep = " ", eq_string, "+")
    
  }
  return(eq_string)
}

x = c(8, 9, 11, 12)
y = c(0.9031, 0.9542, 1.0414, 1.0792)
x = c(0, 8, 16, 24, 32, 40)
y = c(14.621, 11.843, 9.870, 8.418, 7.305, 6.413)

interpolating_polynomial = NDD(x, y, (length(x) - 1))
print(interpolating_polynomial$func(27))
print(interpolating_polynomial$m)


