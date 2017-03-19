library(ROI)
#' @importFrom ROI OP
#' @importFrom ROI L_constraint
#' @importFrom ROI V_bound
#' @importFrom ROI ROI_solve
.solve <- function(varsIn, constraints, obj, sense) {
  varData <- .expand.vars(varsIn)
  vars <- varData$names
  m <- length(constraints)
  n <- length(vars)
  A <- matrix(0, nrow = m, ncol = n)
  rownames(A) <- c(1:m)
  colnames(A) <- vars
  comps <- character(length = m)
  b <- numeric(length = m)
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

  li <- which(varData$bounds$lower != 0)
  lb <- varData$bounds$lower[li]
  ui <- which(varData$bounds$upper != Inf)
  ub <- varData$bounds$upper[ui]

  bounds <- NULL
  if (length(li) == 0) {
    if (length(ui) > 0) {
      bounds <- V_bound(ui = ui, ub = ub)
    }
  } else {
    if (length(ui) > 0) {
      bounds <- V_bound(li, ui, lb, ub)
    } else {
      bounds <- V_bound(li = li, lb = lb)
    }
  }

  prob <- OP(objective = matO,
            constraints = L_constraint(L = A,
                                       dir = comps,
                                       rhs = b),
            maximum = (sense == "max"),
            bounds = bounds,
            types = varData$type
            )
 soln <- ROI_solve(prob)

 if (soln$status$code == 0) {
   .populateGlobal(vars, soln)
   .synchArrays(varsIn)
 } else {
   warning(soln$status$msg$message)
 }
return(soln)
}

.expand.vars <- function(vars) {
  varOut <- list(names = character(),
                 bounds = list(lower = numeric(),
                               upper = numeric()),
                 type = character())
  for (varName in vars) {
    var <- eval.parent(parse(text = varName), n = 3)
    if ("ramlVariable" %in% class(var)) {
      varOut$names <- c(varOut$names, var@name)
      varOut$bounds$lower <- c(varOut$bounds$lower, var@bounds[1])
      varOut$bounds$upper <- c(varOut$bounds$upper, var@bounds[2])
      varOut$type <- c(varOut$type, ifelse(var@integer == "Binary", "B",
                                    ifelse(var@integer == "Integer", "I", "C")))
    } else if ("ramlArray" %in% class(var)) {
      for (index in var@indicies) {
        varOut$names <- c(varOut$names, paste0(var@name, index))
        varOut$bounds$lower <- c(varOut$bounds$lower, var@bounds[1])
        varOut$bounds$upper <- c(varOut$bounds$upper, var@bounds[2])
        varOut$type <- c(varOut$type, ifelse(var@integer == "Binary", "B",
                                      ifelse(var@integer == "Integer", "I", "C")))
      }
    }
  }
  return(varOut)
}

.populateGlobal <- function(vars, soln) {
  for (var in vars) {
    eval.parent(parse(text = paste0(var, "@value <- ", soln$solution[var])), n = 3)
  }
}

.synchArrays <- function(vars) {
  for (var in vars) {
    var <- eval.parent(parse(text = var), n = 3)
    if ("ramlArray" %in% class(var)) {
      out <- numeric(length(var@indicies))
      for (i in 1:length(var@indicies)) {
        out[i] <- eval.parent(parse(text = paste0(var@name, var@indicies[i], "@value")), n = 3)
      }
      eval.parent(parse(text = paste0(var@name, "@value <- c(", paste0(out, collapse = ","), ")")), n = 3)
    }
  }
}
