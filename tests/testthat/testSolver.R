m <- Model()

m$var(x)
m$var(y)

m$objective(x + y)
m$constraint(x + y <= 2)
m$constraint(x <= 1)
m$sense = "max"

m$solve()
expect_equal(x@value, 1)
expect_equal(y@value, 1)
