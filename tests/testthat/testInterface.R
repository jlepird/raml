m <- Model()

m$var(x >= 0)

expect_s4_class(x, "ramlVariable")
expect_equal(x@bounds[1], 0)
expect_equal(x@bounds[2], Inf)
expect_equal(x@name, "x")
expect_equal(x@integer, "Real")
expect_equal(capture.output(x), "x ∈ ℝ ∩ [0, Inf)")

m$var(y <= 10)
expect_s4_class(y, "ramlVariable")
expect_equal(y@bounds[1],-Inf)
expect_equal(y@bounds[2], 10)
expect_equal(y@name, "y")
expect_equal(y@integer, "Real")
expect_equal(capture.output(y), "y ∈ ℝ ∩ (-Inf, 10]")

m$var(z)
expect_s4_class(z, "ramlVariable")
expect_equal(z@bounds[1],-Inf)
expect_equal(z@bounds[2], Inf)
expect_equal(z@name, "z")
expect_equal(z@integer, "Real")
expect_equal(capture.output(z), "z ∈ ℝ")

m$var(a[1:10] >= 0)
expect_s4_class(a, "ramlArray")
expect_s4_class(a_1, "ramlVariable")
expect_s4_class(a_10, "ramlVariable")
expect_equal(a@bounds[1], 0)
expect_equal(a@bounds[2], Inf)
expect_equal(a@name, "a")
expect_equal(a@integer, "Real")
expect_equal(length(a@indicies), 10)
expect_equal(capture.output(a), "a[i] ∈ ℝ ∩ [0, Inf) ∀ i ∈ 1:10")

m$var(b[1:10, 1:10] >= 0)
expect_s4_class(b, "ramlArray")
expect_s4_class(b_1_1, "ramlVariable")
expect_s4_class(b_10_10, "ramlVariable")
expect_equal(b@bounds[1], 0)
expect_equal(b@bounds[2], Inf)
expect_equal(b@name, "b")
expect_equal(b@integer, "Real")
expect_equal(length(b@indicies), 100)
expect_equal(capture.output(b), "b[i] ∈ ℝ ∩ [0, Inf) ∀ i ∈ 1:10 Ⓧ 1:10")

m$var(bin, integer = "Binary")
expect_equal(bin@integer, "Binary")
expect_equal(capture.output(bin), "bin ∈ {0, 1}")

m$var(int, integer = "Integer")
expect_equal(int@integer, "Integer")
expect_equal(capture.output(int), "int ∈ ℤ")

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
