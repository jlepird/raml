m <- Model()

m$var(x >= 0)

expect_named(x)
expect_equal(x$bounds[1], 0)
expect_equal(x$bounds[2], Inf)
expect_equal(x$name, "x")
expect_equal(x$integer, "Real")

m$var(y <= 10)
expect_named(y)
expect_equal(y$bounds[1],-Inf)
expect_equal(y$bounds[2], 10)
expect_equal(y$name, "y")
expect_equal(y$integer, "Real")

m$var(z)
expect_named(z)
expect_equal(z$bounds[1],-Inf)
expect_equal(z$bounds[2], Inf)
expect_equal(z$name, "z")
expect_equal(z$integer, "Real")
