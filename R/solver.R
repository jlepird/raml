require("lpSolve")
#' @importFrom lpSolve lp
.solve <- function(vars, constraints, obj, sense) {
  vars <- .expand.vars(vars)
  m <- length(constraints)
  n <- length(vars)
  A <- matrix(0, nrow = m, ncol = n)
  rownames(A) <- c(1:m)
  colnames(A) <- vars
  comps <- character(length = m)
  b <- numeric(length = m)
  intVars <- numeric()
  binVars <- numeric()
  matO <- numeric(length = length(vars))
  names(matO) <- vars
  if (length(obj@coefs) > 0) {
    for (i in 1:length(obj@coefs)) {
      matO[obj@vars[i]] <- obj@coefs[i]
    }
  }
  for (i in 1:m) {
    lhs <- constraints[[i]]@lhs - constraints[[i]]@rhs
    b[i] <- -1 * lhs@offset
    for (j in 1:length(lhs@coefs)) {
      A[i, lhs@vars[j]] <- lhs@coefs[j]
    }
    comps[i] <- constraints[[i]]@comp
  }

  soln <- lp(objective.in = matO,
            const.mat = A,
            const.dir = comps,
            const.rhs = b,
            direction = sense
            )
 .populateGlobal(vars, soln)
 return(soln)
}

.expand.vars <- function(vars) {
  varOut <- character()
  for (varName in vars) {
    var <- eval.parent(parse(text = varName), n = 3)
    if ("ramlVariable" %in% class(var)) {
      varOut <- c(varOut, var@name)
    } else if ("ramlArray" %in% class(var)) {
      for (index in var@indicies) {
        varOut <- c(varOut, paste0(var@name, index))
        }
    }
  }
  return(varOut)
}

.populateGlobal <- function(vars, soln) {
  for (i in 1:length(vars)) {
    eval.parent(parse(text = paste0(vars[i], "@value <- ", soln$solution[i])), n = 3)
  }
}

