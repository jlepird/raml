#' A class that holds all the necessary data to build a linear programming model.
#' @name RAMLModel
#' @import methods
#' @field sense Should the objective be minimized ("min") or maximized ("max")?
#' @field soln An object of class \code{"OP_solution"} that contains information about the problem solution.
#' @field .__obj (Internal use only) The objective function of the model.
#' @field .__constraints (Internal use only) A list of constraints of the model.
#' @field .__variables (Internal use only) An array of the variables declared in the model.
.Model <- setRefClass("RAMLModel",
                     fields = c("sense",
                                "soln",
                                ".__obj",
                                ".__constraints",
                                ".__variables"),
                     methods = list(
                       initialize = function(){

                         # Provide default values
                         sense          <<- "min"
                         .__obj         <<- NULL
                         .__constraints <<- list()
                         .__variables   <<- NULL
                         soln           <<- NULL
                       },
                       show = function() {
                         if (sense == "min"){
                           cat("Minimize: ")
                         } else if (sense == "max"){
                           cat("Maximize: ")
                         }
                         if (is.null(.__obj)) {
                           cat("(Undefined objective function)\n")
                         } else .showAffineExpr(.__obj, new.line = TRUE)
                         cat("Subject to:\n")
                         if (length(.__constraints) > 0) {
                           for (i in 1:length(.__constraints)) {
                             cat("\t")
                             .showComparison(.__constraints[[i]])
                             cat("\n")
                           }
                         } else {
                           cat("(No defined constraints)\n")
                         }
                       },
                       var = function(defn, integer = "Real"){
                         defn <- match.call()$defn
                         bounds <- c(-Inf, Inf)

                         if (length(defn) > 1) {
                           # First, figure out if the user defined var bounds.
                           equality <- as.character(defn[1])

                           if (equality %in% c(">=", "<=")) {
                             if (as.character(defn[1]) == ">=") {
                               # Get right hand side
                               lb <- eval(parse(text = defn[3]))
                               bounds <- c(lb, Inf)
                             } else if (as.character(defn[1]) == "<=") {
                               ub <- eval(parse(text = defn[3]))
                               bounds <- c(-Inf, ub)
                             }
                            defn <- defn[2][[1]]
                           }
                         }

                         if (length(defn) > 1) {
                           if (as.character(defn[[1]]) == "[") { # then it's an array
                             name <- as.character(defn[[2]])
                             indexDisplay <- paste(as.character(defn[3:length(defn)]), collapse = " \U24CD ")
                             txt <- paste0(name, " <- raml:::.defArray(\"", name, "\", c(", bounds[1], ",", bounds[2], ")", ",\"", integer, "\",", list(lapply(defn[3:length(defn)], eval)), ",\"", indexDisplay, "\")")
                             .__variables <<- c(.__variables, name)
                             eval.parent(parse(text = txt))
                           } else {
                             stop(paste("Unknown error parsing", as.character(defn)))
                           }
                         } else {
                             name <- as.character(defn)
                             txt <- paste0(name, " <- raml:::.defVar(\"", name, "\", c(", bounds[1], ",", bounds[2], ")", ",\"", integer, "\")")
                             .__variables <<- c(.__variables, name)
                             eval.parent(parse(text = txt))
                         }
                         invisible()
                       },
                       constraint = function(expr) {
                         .__constraints <<- append(.__constraints, expr)
                       },
                       objective = function(expr){
                         .__obj <<- .toAffineExpr(expr)
                       },
                       solve = function(...) {
                         #' @param ... Additional arguments to pass to the ROI_solve method.
                          soln <<- .solve(.__variables, .__constraints, .__obj, sense, ...)
                          cat(paste0("Optimal solution found.\nThe objective value is: ", soln$objval, "\n"))
                       }
                      )
)

#' Create an optimization model.
#' @description
#'  Creates a object that will hold your optimization model.
#'  You can use the methods of this object to define your optimization problem.
#' @export
#' @examples
#' m <- Model()
#' m$var(x >= 0)
#' m$var(y >= 0)
#' m$constraint(x + y <= 2)
#' m$sense <- "max"
#' m$objective(2*x + y)
#' m$solve()
#' value(x)
Model <- function() .Model()

#' An abstract class that represents anything that conforms to algebraic rules.
#' @exportClass AbstractRamlAlgObject
setClass("AbstractRamlAlgObject",
         contains = "VIRTUAL")

#' An abstract class that represents arrays or variables that conform to algebraic rules.
#' @exportClass ramlAlgObject
#' @section Slots:
#'  \describe{
#'    \item{\code{name}:}{Object of class \code{"character"}-- the name of the variable in the user's scope.}
#'    \item{\code{bounds}:}{Object of class \code{"numeric"}, which defines the lower and upper bounds of the variable's domain.}
#'    \item{\code{integer}:}{Object of class \code{"character"}, which defines if the variable is a \code{"Real"}, \code{"Integer"}, or \code{"Binary"}}
#'    \item{\code{value}:}{Object of class \code{"numeric"}, the value of the variable at the model's optimum.}
#'  }
setClass("ramlAlgObject",
         representation(name    = "character",
                        bounds  = "numeric",
                        integer = "character",
                        value   = "numeric"),
         contains = c("AbstractRamlAlgObject", "VIRTUAL"))

#' A class that represents a single variable
#' @exportClass ramlVariable
#' @section Slots:
#'  \describe{
#'    \item{\code{parent}:}{Object of class \code{"character"}-- the name of the array this variable is a part of. Array of length 0 if variable is not part of array..}
#'  }
setClass("ramlVariable",
         representation(parent = "character"),
         prototype(parent = character()),
         contains = "ramlAlgObject")

.defVar <- function(name, bounds, integer, parent = character()){
  out <- new("ramlVariable",
              name = name,
              bounds = bounds,
              integer = integer,
              value = NA_real_,
              parent = parent)
  return(out)
}

#' A class that represents a variable
#' @exportClass ramlArray
#' @section Slots:
#'  \describe{
#'    \item{\code{indicies}:}{Objectx of class \code{"character"}-- a vector of the suffixes appended to this variable for its elements.}
#'    \item{\code{indexDisplay}:}{Objectx of class \code{"character"}-- human-readable display of these indicies.}
#'  }
setClass("ramlArray",
         representation(indicies     = "character",
                        indexDisplay = "character"),
         contains = "ramlAlgObject")

#' Defines a variable in the current scope
.defArray <- function(name, bounds, integer, indicies, indexDisplay){
  all.indicies <- expand.grid(indicies)
  all.indicies$index <- apply(all.indicies, 1, function(x) paste0("_", paste(x, collapse = "_")))
  out <- new("ramlArray",
              name         = name,
              bounds       = bounds,
              integer      = integer,
              indicies     = all.indicies$index,
              indexDisplay = indexDisplay,
              value        = NA_real_)

  for (i in 1:nrow(all.indicies)) {
    # raml:::.defVar(paste0(name, all.indicies$index[i], bounds, integer))
    name2 <- paste0(name, all.indicies$index[i])
    txt <- paste0(name2, " <- raml:::.defVar(\"", name2 , "\", c(", bounds[1], ",", bounds[2], ")", ",\"", integer, "\",\"", name, "\")")
    eval.parent(parse(text = txt))
  }
  return(out)
}

#' Natural subsetting for indicies
#' @export
#' @rdname raml-algebra
#' @param x The object of \code{"ramlArray"} to be subsetted.
#' @param i The first index.
#' @param j The second index (if needed).
#' @param ... Additional indicies.
#' @param drop (Not used).
setMethod("[", signature(x = "ramlArray"), function(x, i, j, ..., drop = TRUE) {
    dimen <- length(strsplit(gsub(x@name, "foo", x@indicies[1]), "_")[[1]]) - 1
    if (missing(j)) {
      if (dimen != 1) stop(paste0("Wrong number of indicies-- expected ", dimen, ", got 1."))
      return(eval.parent(parse(text = paste0(x@name, "_", i))))
    } else if (missing(...)) {
      if (dimen != 2) stop(paste0("Wrong number of indicies-- expected ", dimen, ", got 2."))
      return(eval.parent(parse(text = paste0(x@name, "_", i, "_", j))))
    } else {
      if (dimen != 2 + length(list(...))) stop(paste0("Wrong number of indicies-- expected ", dimen, ", got ", 2 + length(list(...)), "."))
      return(eval.parent(parse(text = paste0(x@name, "_", i, "_", j, "_", paste(..., collapse = "_")))))
    }
})

#' Nice display for single variables.
#' @param object The variable to be printed, of the "ramlVariable" class.
#' @param new.line Should the print add a new line at the end?
#' @param in.array Is the variable actually an array?
.printVar <- function(object, new.line = TRUE, is.array = FALSE){
  lbBracket <- "["
  if (object@bounds[1] == -Inf) {
    lbBracket <- "("
  }
  ubBracket <- "]"
  if (object@bounds[2] == Inf) {
    ubBracket <- ")"
  }
  if (object@integer == "Real") {
    field <- "\U211D" # doubleR
  } else if (object@integer == "Integer") {
    field <- "\U2124" # doubleZ
  } else if (object@integer == "Binary") {
    field <- "{0, 1}"
  }

  if (sum(object@bounds == c(-Inf, Inf)) == 2) {
    boundStatement <- ""
  } else {
    # U2229 = \cap
    boundStatement <- paste0(" \U2229 ",lbBracket, object@bounds[1],", ", object@bounds[2], ubBracket)
  }

  if (!is.array && length(object@parent) > 0) { # then the variable name is part of an array
      name <- gsub(object@parent, "", object@name)
      name <- strsplit(name, "_")[[1]]
      name <- paste0(object@parent, "[", paste(name[2:length(name)], collapse = ","), "]")
  } else {
    name <- object@name
  }

  # U2208 is \in
  cat(paste0(name, " \U2208 ", field, boundStatement))

  if (new.line) {
    cat("\n")
  }
}

#' Nice display for single variables.
#' @export
#' @param object The variable to be printed, of the "ramlVariable" class.
#' @param ... Other arguments to pass on to print.
setMethod("show", "ramlVariable", function(object){
  .printVar(object, new.line = TRUE)
}
)

#' Nice display for arrays.
#' @export
#' @param object The variable to be printed, of the "ramlArray" class.
setMethod("show", "ramlArray", function(object){
  object@name <- paste0(object@name, "[i]")
  .printVar(object, new.line = FALSE, is.array = TRUE)
  cat(paste(" \U2200 i \U2208", object@indexDisplay))
  cat("\n")
}
)

#' A class that represents an affine combination of variables.
#' @exportClass AffineExpr
#' @section Slots:
#'  \describe{
#'    \item{\code{vars}:}{Object of class \code{"character"}-- the variables included in this affine expression}
#'    \item{\code{coefs}:}{Object of class \code{"numeric"}, the scalar coefficients matching the the \code{vars}}
#'    \item{\code{offset}:}{Object of class \code{"numeric"}, which indicates the scalar that offsets this affine expression from a linear expression.}
#'  }
setClass("AffineExpr",
         representation(vars   = "character",
                        coefs  = "numeric",
                        offset = "numeric"),
         contains = "AbstractRamlAlgObject"
         )

#' Nice display for affine expressions.
#' @export
#' @param object The expression to be printed, of the "AffineExpr" class.
setMethod("show", "AffineExpr", function(object) .showAffineExpr(object, new.line = TRUE))

#' Generic for displaying affine expressions
.showAffineExpr <- function(object, new.line = TRUE){
  out <- ""
  if (object@offset != 0 || length(object@coefs) == 0) {
    out <- paste(object@offset)
  }
  if (length(object@coefs) > 0) {
    for (i in 1:length(object@coefs)) {
      if (object@coefs[i] != 0) {
        if (nchar(out) == 0) {
          out <- paste0(object@coefs[i], "*", object@vars[i])
        } else {
          out <- paste0(out, " + ", object@coefs[i], "*",  object@vars[i])
        }
      }
    }
  }
  if (new.line) cat(paste0(out, "\n"))
  else cat(out)
}

#' Algebra in raml
#' @description
#' Algebra within the raml ecosystem behaves exactly as you'd expect it to.
#' @export
#' @rdname raml-algebra
#' @param e1 The first algebraic object.
#' @param e2 The second algebraic object.
#' @examples
#' m <- Model()
#' m$var(x)
#' m$var(y)
#' x + x == 2 * x
#' x - x == 0 * x
#' x + y + x == x + x + y
#' m$objective(x + y)
#' m$constraint(x + 2 * y <= 3)
setMethod("+", signature(e1 = "ramlVariable", e2 = "numeric"), function(e1, e2) {
  return(new("AffineExpr",
             vars = e1@name,
             offset = e2,
             coefs = 1))
})

#' @export
#' @rdname raml-algebra
setMethod("+", signature(e2 = "ramlVariable", e1 = "numeric"), function(e1, e2) e2 + e1)

#' @export
#' @rdname raml-algebra
setMethod("+", signature(e1 = "ramlVariable", e2 = "ramlVariable"), function(e1, e2){
      if (e1@name != e2@name) {
        out <- new("AffineExpr",
                  vars   = c(e1@name, e2@name),
                  offset = 0,
                  coefs  = c(1,1))
      } else {
        out <- new("AffineExpr",
                    vars   = c(e1@name),
                    offset = 0,
                    coefs  = c(2))
      }
})

#' @export
#' @rdname raml-algebra
setMethod("+", signature(e1 = "ramlVariable", e2 = "AffineExpr"), function(e1, e2) {
        if (e1@name %in% e2@vars) {
          e2@coefs[which(e2@vars == e1@name)] <-
            e2@coefs[which(e2@vars == e1@name)] + 1
        } else {
          e2@coefs <- c(e2@coefs, 1)
          e2@vars <-  c(e2@vars,  e1@name)
        }
  return(e2)
})

#' @export
#' @rdname raml-algebra
setMethod("+", signature(e2 = "numeric", e1 = "AffineExpr"), function(e1, e2){
  e1@offset <- e1@offset + e2
  return(e1)
})

#' @export
#' @rdname raml-algebra
setMethod("+", signature(e1 = "numeric", e2 = "AffineExpr"), function(e1, e2){
  return(e2 + e1)
})

#' @export
#' @rdname raml-algebra
setMethod("+", signature(e2 = "ramlVariable", e1 = "AffineExpr"), function(e1, e2) e2 + e1)

#' @export
#' @rdname raml-algebra
setMethod("+", signature(e1 = "AffineExpr", e2 = "AffineExpr"), function(e1, e2){
      if (length(e2@vars) > 0) {
        for (i in 1:length(e2@vars)) {
            if (e2@vars[i] %in% e1@vars) {
              e1@coefs[which(e1@vars == e2@vars[i])] <-
                e1@coefs[which(e1@vars == e2@vars[i])] + e2@coefs[i]
            } else {
              e1@vars <- c(e1@vars, e2@vars[i])
              e1@coefs <- c(e1@coefs, e2@coefs[i])
            }
        }
      }
      e1@offset <- e1@offset + e2@offset
      return(e1)
})

#' @export
#' @rdname raml-algebra
setMethod("*", signature(e1 = "ramlVariable", e2 = "numeric"), function(e1, e2) {
  return(new("AffineExpr",
             vars = e1@name,
             offset = 0,
             coefs = e2))
})

#' @export
#' @rdname raml-algebra
setMethod("*", signature(e2 = "numeric", e1 = "AffineExpr"), function(e1, e2){
  e1@coefs <- e1@coefs * e2
  e1@offset <- e1@offset * e2
  return(e1)
})

#' @export
#' @rdname raml-algebra
setMethod("*", signature(e1 = "numeric", e2 = "AbstractRamlAlgObject"), function(e1, e2){
  return(e2 * e1)
})

#' @export
#' @rdname raml-algebra
setMethod("-", signature(e1 = "AbstractRamlAlgObject", e2 = "numeric"), function(e1, e2) e1 + (-1 * e2))

#' @export
#' @rdname raml-algebra
setMethod("-", signature(e2 = "AbstractRamlAlgObject", e1 = "numeric"), function(e1, e2) e1 + (-1 * e2))

#' @export
#' @rdname raml-algebra
setMethod("-", signature(e1 = "AbstractRamlAlgObject", e2 = "AbstractRamlAlgObject"), function(e1, e2) e1 + (-1 * e2))

#' @export
#' @rdname raml-algebra
setMethod("/", signature(e2 = "numeric", e1 = "AbstractRamlAlgObject"), function(e1, e2) {
  return(e1 * (1/e2))
})

#' A class that represents an affine combination of variables.
#' @exportClass ramlComparison
#' @section Slots:
#'  \describe{
#'    \item{\code{lhs}:}{Object of class \code{"AffineExpr"}, the left-hand side of the comparison.}
#'    \item{\code{rhs}:}{Object of class \code{"AffineExpr"}, the rightt-hand side of the comparison.}
#'    \item{\code{comp}:}{Object of class \code{"character"}, which indicates if the Comparison being made is \code{">="}, \code{"<="}, or \code{"=="}.}
#'  }
setClass("ramlComparison",
         representation(lhs  = "AffineExpr",
                        rhs  = "AffineExpr",
                        comp = "character")
)

#' Nice display for algabraic comparisons.
#' @export
#' @param object The Comparison expression to be displayed.
setMethod("show", signature("ramlComparison"), function(object) .showComparison(object))

# Private function for displaying comparisions. Also used in the model printing routine.
.showComparison <- function(object) {
  .showAffineExpr(object@lhs, new.line = FALSE)
  cat(paste0(" ", object@comp, " "))
  .showAffineExpr(object@rhs, new.line = FALSE)
}

#' Function to coerce objects into affine expressions.
setGeneric(".toAffineExpr", function(object) standardGeneric(".toAffineExpr"))

#' Converts a single number into an affine expression.
setMethod(".toAffineExpr", signature("numeric"), function(object){
  if (length(object) == 1) { # Make sure the user isn't doing something weird
    return(new("AffineExpr",
               coefs = numeric(),
               vars  = character(),
               offset = object))
  } else stop(paste("Expected argument to be of length 1, got", length(object), "instead."))
})

#' Converts a algebraic object into an affine expression.
setMethod(".toAffineExpr", signature("AbstractRamlAlgObject"), function(object){
  if (length(object) == 1) { # Make sure the user isn't doing something weird
    return(1 * object)
  } else stop(paste("Expected argument to be of length 1, got", length(object), "instead."))
})

#' Dummy function to convert an affine expression into itself.
setMethod(".toAffineExpr", signature("AffineExpr"), function(object){
  if (length(object) == 1) { # Make sure the user isn't doing something weird
    return(object)
  } else stop(paste("Expected argument to be of length 1, got", length(object), "instead."))
})

#' @export
#' @rdname raml-algebra
setMethod(">=", signature(e1 = "ANY", e2 = "AbstractRamlAlgObject"), function(e1, e2){
  return(new("ramlComparison",
             lhs = .toAffineExpr(e1),
             rhs = .toAffineExpr(e2),
             comp = ">="))
})

#' @export
#' @rdname raml-algebra
setMethod("<=", signature(e1 = "ANY", e2 = "AbstractRamlAlgObject"), function(e1, e2){
  return(new("ramlComparison",
             lhs = .toAffineExpr(e1),
             rhs = .toAffineExpr(e2),
             comp = "<="))
})

#' @export
#' @rdname raml-algebra
setMethod("==", signature(e1 = "ANY", e2 = "AbstractRamlAlgObject"), function(e1, e2){
  return(new("ramlComparison",
             lhs = .toAffineExpr(e1),
             rhs = .toAffineExpr(e2),
             comp = "=="))
})

#' @export
#' @rdname raml-algebra
setMethod(">=", signature(e2 = "ANY", e1 = "AbstractRamlAlgObject"), function(e1, e2){
  return(new("ramlComparison",
             lhs = .toAffineExpr(e1),
             rhs = .toAffineExpr(e2),
             comp = ">="))
})

#' @export
#' @rdname raml-algebra
setMethod("<=", signature(e2 = "ANY", e1 = "AbstractRamlAlgObject"), function(e1, e2){
  return(new("ramlComparison",
             lhs = .toAffineExpr(e1),
             rhs = .toAffineExpr(e2),
             comp = "<="))
})

#' @export
#' @rdname raml-algebra
setMethod("==", signature(e2 = "ANY", e1 = "AbstractRamlAlgObject"), function(e1, e2){
  return(new("ramlComparison",
             lhs = .toAffineExpr(e1),
             rhs = .toAffineExpr(e2),
             comp = "=="))
})

#' Totally redundent, but suppresses warnings when two variables are compared.
#' @export
#' @rdname raml-algebra
setMethod("==", signature(e2 = "AbstractRamlAlgObject", e1 = "AbstractRamlAlgObject"), function(e1, e2){
  return(new("ramlComparison",
             lhs = .toAffineExpr(e1),
             rhs = .toAffineExpr(e2),
             comp = "=="))
})


#' Totally redundent, but suppresses warnings when two variables are compared.
#' @export
#' @rdname raml-algebra
setMethod("<=", signature(e2 = "AbstractRamlAlgObject", e1 = "AbstractRamlAlgObject"), function(e1, e2){
  return(new("ramlComparison",
             lhs = .toAffineExpr(e1),
             rhs = .toAffineExpr(e2),
             comp = "<="))
})


#' Totally redundent, but suppresses warnings when two variables are compared.
#' @export
#' @rdname raml-algebra
setMethod(">=", signature(e2 = "AbstractRamlAlgObject", e1 = "AbstractRamlAlgObject"), function(e1, e2){
  return(new("ramlComparison",
             lhs = .toAffineExpr(e1),
             rhs = .toAffineExpr(e2),
             comp = ">="))
})



#' Vector inner products.
#' @description Takes the inner product of a variable array and a numeric array.
#' @export
#' @param a An array of class \code{"numeric"}.
#' @param b An array of variables.
#' @rdname dots
#' @examples
#' m <- Model()
#' m$var(x[1:3] >= 0)
#' m$constraint(dot(x, c(1,1,1)) >= 1)
#' m$objective(dot(c(1,2,3), x))
#' show(m)
setGeneric("dot", function(a, b) standardGeneric("dot"))

#' @export
#' @rdname dots
setMethod("dot", signature(a = "numeric", b = "ramlArray"), function(a, b) {
  length(a) == length(b@indicies) ||
    stop(paste0("Dimension mismatch: length of dot product arguments must match. Got length(numeric) = ", length(a), " and length(array) = ", length(b@indicies)))

  indx <- b@indicies[1]

  length(gsub("_", "", indx)) != length(indx) - 1 ||
    stop("Dot product only defined for one-dimensional vectors.")

  out <- 0
  for (i in 1:length(a)) {
    out <- out + a[i] * eval.parent(parse(text = paste0(b@name, b@indicies[i])))
  }
  return(out)
})

#' @export
#' @rdname dots
setMethod("dot", signature(b = "numeric", a = "ramlArray"), function(a, b) {
  # Due to issues with variables existing in the right scope, we can't just
  # call dot(b, a), as eval.parent would call *here*, not where it should.
  tmp <- a
  a   <- b
  b   <- tmp

  length(a) == length(b@indicies) ||
    stop(paste0("Dimension mismatch: length of dot product arguments must match. Got length(numeric) = ", length(a), " and length(array) = ", length(b@indicies)))

  indx <- b@indicies[1]

  length(gsub("_", "", indx)) != length(indx) - 1 ||
    stop("Dot product only defined for one-dimensional vectors.")

  out <- 0
  eval.parent(parse(text = "print(ls())"))
  for (i in 1:length(a)) {
    out <- out + a[i] * eval.parent(parse(text = paste0(b@name, b@indicies[i])))
  }
  return(out)
})

#' A helper function that extracts the value of a variable.
#' @param x The object.
#' @export
#' @rdname value
#' @examples
#' m <- Model()
#' m$var(x >= 0)
#' m$objective(x)
#' m$constraint(x >= 0.1)
#' value(x) # NA
#' m$solve()
#' value(x) # 0.1
setGeneric("value", function(x) standardGeneric("value"))

#' @export
#' @rdname value
setMethod("value", signature("ramlAlgObject"), function(x) return(x@value))

#' Extracts the dual value (shadow cost) of a constraint.
#' @param m The model object.
#' @param constr The constraint in question.
#' @exportMethod dual
#' @rdname dual
#' @return The dual value (shadow cost) of a constraint. Returns \code{NA} if there are integer or binary variables in the model.
#' @usage dual(m, constr)
#' @examples
#' m <- Model()
#' m$var(x >= 0)
#' m$objective(x)
#' m$constraint(x >= 0.1)
#' m$solve()
#' dual(m, x >= 0.1)  # 1.0
setGeneric("dual", function(m, constr) standardGeneric("dual"))

#' @exportMethod dual
#' @rdname dual
#' @importFrom utils capture.output
#' @usage dual(m, constr)
setMethod("dual", signature("RAMLModel", "ramlComparison"), function(m, constr) {
  constrID <- which(do.call(c, lapply(m$.__constraints, function(foo) identical(constr, foo))))
  length(constrID) == 1 && return(m$soln$message$auxiliary$dual[constrID])
  stop(paste0("Constraint ", capture.output(constr), " not found in model."))
})

#' Helper function to take sums over variable arrays.
#' @param var The indexed variable to be summed.
#' @param ... The indicies to iterate through.
#' @export
#' @examples
#' m <- Model()
#' m$var(x[1:3] >= 0)
#' rsum(x[i], i = 1:3)  # x[1] + x[2] + x[3]
#' m$var(y[1:2, 1:2] >= 0)
#' rsum(y[i,j], i = 1:2, j = 1:2)  # y[1,1] + y[1,2] + y[2,1] + y[2,2]
#' v <- c(1,2,3)
#' rsum(v[i]*x[i], i = 1:3)  # 1*x[1] + 2*x[2] + 3*x[3]
rsum <- function(var, ...) {

  # Get what the user called.
  args <- match.call(expand.dots = F)

  # Need to iterate over the ... variable to figure out what indicies the user needed.
  indicies <- list()
  for (.name in names(args$...)) {
    indicies[[.name]] <- eval.parent(args$...[[.name]])
    if (eval.parent(parse(text = paste0("exists(\"", .name, "\")")))) {
      stop("Index ", .name, " already exists. Use a different index, or remove that variable by calling rm(", .name, ").")
    }
  }

  # Take the full cartesian product of the indicies.
  all.indices <- expand.grid(indicies)

  # The thing we'll ultimately return.
  out <- 0.0

  # calling this here prevents R's lazy evaluate from ever calling the arguments.
  args.s <- as.character(args)

  # For each value of the cartesian product
  for (i in 1:nrow(all.indices)) {

    # Get the raw index, i.e. x[i]
    varCall <- args.s[2]
    for (var in colnames(all.indices)) {
      # Sub in the variable with the value of that variable.
      eval.parent(parse(text = paste0(var, "<-", all.indices[i, var])))
    }

    # Eval the new arg in the parent env and add it to our return call.
    out <- out + eval.parent(parse(text = varCall))
  }

  # Delete the user-scope vars we created.
  eval.parent(parse(text = paste0("rm(", paste0(colnames(all.indices), collapse = ","), ")")))

  return(out)
}


