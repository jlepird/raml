# raml
[![Travis-CI Build Status](https://travis-ci.org/jlepird/raml.svg?branch=master)](https://travis-ci.org/jlepird/raml)
[![Code Coverage](https://codecov.io/gh/jlepird/raml/branch/master/graph/badge.svg)](https://codecov.io/gh/jlepird/raml)
[![License](https://img.shields.io/npm/l/express.svg)](https://www.r-project.org/Licenses/MIT)

The `raml` package provides an elementary algebraic modeling language (AML) in the R ecosystem.

## Installation

You can install raml from github with:

```R
# install.packages("devtools")
devtools::install_github("jlepird/raml")
```

## Example

This is a basic example which shows you how to solve a common problem:

```R
library(raml)

m <- Model()

m$var(x >= 0)
m$var(y >= 0)

m$objective(x + 2*y)

m$constraint(x + y <= 5)

m$sense <- "max"

m$solve()

## Output:
## Optimal solution found.
## The objective value is: 10

value(x) # 0
value(y) # 5

dual(m, x + y <= 5) # 2.0, the shadow cost of the constraint x + y <= 5.
```
