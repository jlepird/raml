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
    ex <- 0.0
    for (j in 1:12) { 
      ex <- ex + x[i,j,k]
    }
    m$constraint(ex <= 1)
  }
}

## ------------------------------------------------------------------------
for (i in 1:12) {
  for (j in 1:12) {
    if (i != j) {
      ex <- 0.0
      for (k in 1:20) { 
        ex <- ex + x[i,j,k]
      }
      m$constraint(ex >= 1)
    }
  }
}

## ------------------------------------------------------------------------
gamma <- 1.1
ex <- 0.0
for (k in 1:20) {
  for (i in 1:12) { 
    for (j in 1:12) {
      ex <- ex + x[i,j,k] * gamma^k
    }
  }
}
m$objective(ex)

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

