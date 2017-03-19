m <- Model()

m$var(x)
m$var(y)

m$objective(x + y)
m$constraint(x + y <= 2)
m$constraint(x <= 1)
m$sense = "max"

m$solve()
expect_equal(value(x), 1)
expect_equal(value(y), 1)
expect_equal(dual(m, x + y <= 2), 1)
expect_equal(dual(m, x <= 1), 0)
expect_error(dual(m, y <= 2))

m$sense <- "min"
expect_warning(m$solve())

m$sense <- "max"
m$constraint(x >= 2)
expect_warning(m$solve())

m <- Model()
m$var(x >= 0, "Integer")
m$constraint(x <= 1.5)
m$objective(x)
m$sense <- "max"

m$solve()
expect_equal(value(x), 1)

m <- Model()
m$var(x >= 0, "Binary")
m$constraint(x <= 1.5)
m$objective(x)
m$sense <- "max"

m$solve()
expect_equal(value(x), 1)

m <- Model()
m$var(x[1:3] >= 0)
m$objective(dot(rep(1,3), x))
m$constraint(dot(rep(1,3), x) >= 0)
m$sense <- "min"
m$solve()
