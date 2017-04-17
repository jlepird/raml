## ------------------------------------------------------------------------
library(raml)

m <- Model()
m$sense <- "min"

## ------------------------------------------------------------------------
m$var(x[1:12, 1:12, 1:20] >= 0, "Binary")

## ------------------------------------------------------------------------
for (k in 1:20) { 
  for (i in 1:12) {
    for (j in 1:i) {
      m$constraint(x[i, j, k] == x[j, i, k])
    }
  }
}

## ------------------------------------------------------------------------
for (k in 1:20) {
  for (i in 1:12) {
    m$constraint(rsum(x[i,j_i,k], j_i = 1:12) <= 1)
  }
}

## ------------------------------------------------------------------------
for (i in 1:12) {
  for (j in 1:12) {
    if (i != j) {
      m$constraint(rsum(x[i,j,k_i], k_i = 1:20) >= 1)
    }
  }
}

## ------------------------------------------------------------------------
gamma <- 1.1
m$objective(rsum(gamma^k_i * x[i_i, j_i, k_i], i_i = 1:12, j_i = 1:12, k_i = 1:20))

## ------------------------------------------------------------------------
m$solve()

## ---- fig.show='hold', fig.cap = "Conference Schedule.", fig.height=5, fig.width=5----
df <- data.frame(i = numeric(), j = numeric(), k = numeric())
for (i in 1:12) {
  for (j in 1:12) {
    for (k in 1:20) {
      if (value(x[i,j,k]) > 0) {
        df <- rbind(df, data.frame(i = i, j = j, k = k))
      }
    }
  }
}
library(ggplot2)
ggplot(df, aes(i,j, label = k)) + 
  geom_text() + 
  scale_x_continuous("Rep i", breaks = 1:12) + 
  scale_y_continuous("Rep j", breaks = 1:12)

