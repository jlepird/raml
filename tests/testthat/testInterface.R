m <- Model()

m$var(x >= 0)

expect_s4_class(x, "ramlVariable")
expect_equal(x@bounds[1], 0)
expect_equal(x@bounds[2], Inf)
expect_equal(x@name, "x")
expect_equal(x@integer, "Real")
if(.Platform$OS.type == "unix") {
  expect_equal(capture.output(x), "x ∈ ℝ ∩ [0, Inf)")
} else {
  print("Skipping Unicode test on Windows...")
}


expect_error(m$var(x[[]]))

m$var(y <= 10)
expect_s4_class(y, "ramlVariable")
expect_equal(y@bounds[1],-Inf)
expect_equal(y@bounds[2], 10)
expect_equal(y@name, "y")
expect_equal(y@integer, "Real")
if(.Platform$OS.type == "unix") {
  expect_equal(capture.output(y), "y ∈ ℝ ∩ (-Inf, 10]")
} else {
  print("Skipping Unicode test on Windows...")
}

m$var(z)
expect_s4_class(z, "ramlVariable")
expect_equal(z@bounds[1],-Inf)
expect_equal(z@bounds[2], Inf)
expect_equal(z@name, "z")
expect_equal(z@integer, "Real")
if(.Platform$OS.type == "unix") {
  expect_equal(capture.output(z), "z ∈ ℝ")
} else {
  print("Skipping Unicode test on Windows...")
}

m$var(a[1:10] >= 0)
expect_s4_class(a, "ramlArray")
expect_s4_class(a_1, "ramlVariable")
expect_s4_class(a_10, "ramlVariable")
expect_equal(a@bounds[1], 0)
expect_equal(a@bounds[2], Inf)
expect_equal(a@name, "a")
expect_equal(a@integer, "Real")
expect_equal(length(a@indicies), 10)
if(.Platform$OS.type == "unix") {
  expect_equal(capture.output(a), "a[i] ∈ ℝ ∩ [0, Inf) ∀ i ∈ 1:10")
} else {
  print("Skipping Unicode test on Windows...")
}

expect_equal(a[1], a_1)
if(.Platform$OS.type == "unix") {
expect_equal(capture.output(a[1]), "a[1] ∈ ℝ ∩ [0, Inf)")
} else {
  print("Skipping Unicode test on Windows...")
}
expect_error(a[1,1])

m$var(b[1:10, 1:10] >= 0)
expect_s4_class(b, "ramlArray")
expect_s4_class(b_1_1, "ramlVariable")
expect_s4_class(b_10_10, "ramlVariable")
expect_equal(b@bounds[1], 0)
expect_equal(b@bounds[2], Inf)
expect_equal(b@name, "b")
expect_equal(b@integer, "Real")
expect_equal(length(b@indicies), 100)
if(.Platform$OS.type == "unix") {
expect_equal(capture.output(b), "b[i] ∈ ℝ ∩ [0, Inf) ∀ i ∈ 1:10 Ⓧ 1:10")
} else {
  print("Skipping Unicode test on Windows...")
}
expect_equal(b[1,1], b_1_1)
if(.Platform$OS.type == "unix") {
expect_equal(capture.output(b[1,1]), "b[1,1] ∈ ℝ ∩ [0, Inf)")
} else {
  print("Skipping Unicode test on Windows...")
}
expect_error(b[1])

m$var(d[1:2,1:2,1:2])
expect_equal(d[1,1,1], d_1_1_1)
expect_error(d[1,1,1,1,1])

m$var(bin, integer = "Binary")
expect_equal(bin@integer, "Binary")
if(.Platform$OS.type == "unix") {
expect_equal(capture.output(bin), "bin ∈ {0, 1}")
} else {
  print("Skipping Unicode test on Windows...")
}

m$var(int, integer = "Integer")
expect_equal(int@integer, "Integer")
if(.Platform$OS.type == "unix") {
expect_equal(capture.output(int), "int ∈ ℤ")
} else {
  print("Skipping Unicode test on Windows...")
}

x1 <- x + 1
expect_s4_class(x1, "AffineExpr")
expect_equal(x1@vars, "x")
expect_equal(x1@coefs, 1)
expect_equal(x1@offset, 1)
expect_equal(x1, 1 + x)

xx <- x + x
expect_s4_class(xx, "AffineExpr")
expect_equal(xx@vars, "x")
expect_equal(xx@coefs, 2)
expect_equal(xx@offset, 0)

xxx <- x + x + x
expect_s4_class(xxx, "AffineExpr")
expect_equal(xxx@vars, "x")
expect_equal(xxx@coefs, 3)
expect_equal(xxx@offset, 0)

expect_equal((x + x) + x, x + (x + x))
xxxx <- (x + x) + (x + x)
expect_s4_class(xxxx, "AffineExpr")
expect_equal(xxxx@vars, "x")
expect_equal(xxxx@coefs, 4)
expect_equal(xxxx@offset, 0)

xy <- x + y
expect_s4_class(xy, "AffineExpr")
expect_equal(xy@vars, c("x", "y"))
expect_equal(xy@coefs, c(1, 1))
expect_equal(xy@offset, 0)

xxy <- x + x + y
expect_s4_class(xxy, "AffineExpr")
expect_equal(xxy@vars, c("x", "y"))
expect_equal(xxy@coefs, c(2, 1))
expect_equal(xxy@offset, 0)

xx1 <- x + x + 1
expect_s4_class(xx1, "AffineExpr")
expect_equal(xx1@vars, "x")
expect_equal(xx1@coefs, 2)
expect_equal(xx1@offset, 1)

expect_equal(x + x + 1, 1 + (x + x))

xyxy <- (x + y) + (x + y)
expect_s4_class(xyxy, "AffineExpr")
expect_equal(xyxy@vars, c("x", "y"))
expect_equal(xyxy@coefs, c(2, 2))
expect_equal(xyxy@offset, 0)

xyxz <- (x + y) + (x + z)
expect_s4_class(xyxz, "AffineExpr")
expect_equal(xyxz@vars, c("x", "y", "z"))
expect_equal(xyxz@coefs, c(2, 1, 1))
expect_equal(xyxz@offset, 0)

expect_equal(x - 1, -1 + x)
expect_equal((x - x)@coefs, 0)
expect_equal(1 - x, -1 * (x - 1))
expect_equal(x + x + x, 3 * x)
expect_equal(x / 2, x * 1/2)

expect_equal(capture.output(x + x),     "2*x")
expect_equal(capture.output(x + x + y), "2*x + 1*y")
expect_equal(capture.output(x - x + y), "1*y")
expect_equal(capture.output(1 + x + y), "1 + 1*x + 1*y")

expect_equal(capture.output(1 >= x), "1 >= 1*x")
expect_equal(capture.output(1 <= x), "1 <= 1*x")
expect_equal(capture.output(1 == x), "1 == 1*x")
expect_equal(capture.output(x >= 1), "1*x >= 1")
expect_equal(capture.output(x <= 1), "1*x <= 1")
expect_equal(capture.output(x == 1), "1*x == 1")
expect_equal(capture.output(x + y >= 0), "1*x + 1*y >= 0")

expect_equal(capture.output(m), c("Minimize: (Undefined objective function)", "Subject to:", "(No defined constraints)"))

m$objective(x + y)
expect_equal(capture.output(m), c("Minimize: 1*x + 1*y", "Subject to:", "(No defined constraints)"))

m$constraint(x + y >= 0)
expect_equal(capture.output(m), c("Minimize: 1*x + 1*y", "Subject to:", "\t1*x + 1*y >= 0"))

m$sense <- "max"
expect_equal(capture.output(m), c("Maximize: 1*x + 1*y", "Subject to:", "\t1*x + 1*y >= 0"))

m$constraint(x + y >= 1)
expect_equal(capture.output(m), c("Maximize: 1*x + 1*y", "Subject to:", "\t1*x + 1*y >= 0", "\t1*x + 1*y >= 1"))

expect_equal(dot(rep(1, 10), a), dot(a, rep(1, 10)))
m$var(ary[1:3] >= 0)
expect_equal(dot(c(1,2,3), ary), 1 * ary[1] + 2 * ary[2] + 3 * ary[3])

expect_equal(rsum(a[i], i = 1:2), a[1] + a[2])
expect_equal(rsum(b[i,j], i = 1:2, j = 1:2), b[1,1] + b[2,1] + b[1,2] + b[2,2])
expect_false(exists("i"))
i <- 1
expect_error(rsum(a[i], i = 1:2))
rm(i)

# Validate array calls occur in the correct namespace.
ary <- c(1,2)
f <- function() {
  ary <- c(3,4)
  expect_equal(rsum(a[i]*ary[i], i = 1:2), 3*a[1] + 4*a[2])
}
f()

# Validate sum works in general case of dimension (1-2 hard-coded for speed)
m$var(X[1:1, 1:1, 1:1, 1:1] >= 0)
expect_equal(rsum(X[i, j, k, l], i = 1:1, j = 1:1, k = 1:1, l = 1:1), 1*X[1,1,1,1])
