#' A class that holds all the necessary data to build a linear programming model.
#' @name RAMLModel
#' @import methods
#' @field sense Should the objective be minimized ("min") or maximized ("max")?
.Model <- setRefClass("RAMLModel",
                     fields = c("sense",
                                "obj",
                                "constraints",
                                "variables"),
                     methods = list(
                       initialize = function(...){

                         # Provide default values
                         sense       <<- "min"
                         obj         <<- NULL
                         constraints <<- NULL
                         variables   <<- NULL

                         # Call super to override any defaults
                         callSuper(...)
                       },
                       show = function(){},
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
                             variables <<- c(variables, name)
                             eval.parent(parse(text = txt))
                           } else {
                             stop(paste("Unknown error parsing", as.character(defn)))
                           }
                         } else {
                             name <- as.character(defn)
                             txt <- paste0(name, " <- raml:::.defVar(\"", name, "\", c(", bounds[1], ",", bounds[2], ")", ",\"", integer, "\")")
                             variables <<- c(variables, name)
                             eval.parent(parse(text = txt))
                         }
                         invisible()
                       },
                       constraint = function(){},
                       objective = function(){}
                      )
)

#' Factory for creating Models.
#' @export
#' @param ... Arguments to be passed to the
Model <- function(...) .Model(...)

#' A class that represents a variable
#' @exportClass ramlVariable
#' @section Slots:
#'  \describe{
#'    \item{\code{name}:}{Objectx of class \code{"character"}-- the name of the variable in the user's scope.}
#'    \item{\code{bounds}:}{Object of class \code{"numeric"}, which defines the lower and upper bounds of the variable's domain.}
#'    \item{\code{integer}:}{Object of class \code{"character"}, which defines if the variable is a \code{"Real"}, \code{"Integer"}, or \code{"Binary"}}
#'    \item{\code{value}:}{Object of class \code{"numeric"}, the value of the variable at the model's optimum.}
#'  }
setClass("ramlVariable",
         representation(name    = "character",
                        bounds  = "numeric",
                        integer = "character",
                        value   = "numeric"))

.defVar <- function(name, bounds, integer){
  out <- new("ramlVariable",
              name = name,
              bounds = bounds,
              integer = integer,
              value = NA_real_)
  return(out)
}

#' A class that represents a variable
#' @exportClass ramlArray
#' @section Slots:
#'  \describe{
#'    \item{\code{name}:}{Objectx of class \code{"character"}-- the name of the variable in the user's scope.}
#'    \item{\code{bounds}:}{Object of class \code{"numeric"}, which defines the lower and upper bounds of the variable's domain.}
#'    \item{\code{integer}:}{Object of class \code{"character"}, which defines if the variable is a \code{"Real"}, \code{"Integer"}, or \code{"Binary"}}
#'    \item{\code{indicies}:}{Objectx of class \code{"character"}-- a vector of the suffixes appended to this variable for its elements.}
#'    \item{\code{indexDisplay}:}{Objectx of class \code{"character"}-- human-readable display of these indicies.}
#'    \item{\code{value}:}{Object of class \code{"numeric"}, the value of the variable at the model's optimum.}
#'  }
setClass("ramlArray",
         representation(name         = "character",
                        bounds       = "numeric",
                        integer      = "character",
                        indicies     = "character",
                        indexDisplay = "character",
                        value        = "numeric"))

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
    txt <- paste0(name2, " <- raml:::.defVar(\"", name2 , "\", c(", bounds[1], ",", bounds[2], ")", ",\"", integer, "\")")
    eval.parent(parse(text = txt))
  }
  return(out)
}

#' Nice display for single variables.
#' @param object The variable to be printed, of the "ramlVariable" class.
#' @param new.line Should the print add a new line at the end?
.printVar <- function(object, new.line = TRUE){
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

  # U2208 is \in
  cat(paste0(object@name, " \U2208 ", field, boundStatement))

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
  .printVar(object, new.line = FALSE)
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
         representation(vars    = "character",
                        coefs  = "numeric",
                        offset = "numeric")
         )

#' Adds a scalar to a variable, resulting in an affine expression.
#' @export
#' @rdname var-scalar
#' @param e1 The first variable to be added.
#' @param e2 The second variable to be added.
setMethod("+", signature(e1 = "ramlVariable", e2 = "numeric"), function(e1, e2) {
  return(new("AffineExpr",
             vars = e1@name,
             offset = e2,
             coefs = 1))
})

#' @export
#' @rdname var-scalar
setMethod("+", signature(e2 = "ramlVariable", e1 = "numeric"), function(e1, e2) e2 + e1)

#' Adds two variables together to form an affine expression.
#' @export
#' @param e1 The first variable to be added.
#' @param e2 The second variable to be added.
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
#' @rdname var-affine
#' @param e1 The AffineExpr to be added.
#' @param e2 The variable to be added to the affine expression.
setMethod("+", signature(e1 = "ramlVariable", e2 = "AffineExpr"), function(e1, e2) {
        if (e1@name %in% e2@vars) {
          e2@coefs[which(e2@vars == e1@name)] <-
            e2@coefs[which(e2@vars == e1@name)] + 1
        } else {
          e2@coefs <- c(e2$coefs, 1)
          e2@vars <-  c(e2$vars,  e1@name)
        }
  return(e2)
})

#' Adds a scalar to an affine expression.
#' @export
#' @rdname scalar-affine
#' @param e1 The AffineExpr to be added.
#' @param e2 The scalar to be added to the affine expression.
setMethod("+", signature(e2 = "numeric", e1 = "AffineExpr"), function(e1, e2){
  e1@offset <- e1@offset + e2
  return(e1)
})

#' Adds a scalar to an affine expression.
#' @export
#' @rdname scalar-affine
setMethod("+", signature(e1 = "numeric", e2 = "AffineExpr"), function(e1, e2){
  return(e2 + e1)
})

#' Adds a variable to an affine expression.
#' @export
#' @rdname var-affine
setMethod("+", signature(e2 = "ramlVariable", e1 = "AffineExpr"), function(e1, e2) e2 + e1)

#' Adds two affine expressions together using basic algebra.
#' @export
#' @param e1 The first AffineExpr to be added.
#' @param e2 The second AffineExpr
setMethod("+", signature(e1 = "AffineExpr", e2 = "AffineExpr"), function(e1, e2){
      for (i in 1:length(e2@vars)) {
          if (e2@vars[i] %in% e1@vars) {
            e1@coefs[which(e1@vars == e2@vars[i])] <-
              e1@coefs[which(e1@vars == e2@vars[i])] + e2@coefs[i]
          } else {
            e1@vars <- c(e1@vars, e2@vars[i])
            e1@coefs <- c(e1@coefs, e2@coefs[i])
          }
        }
        e1@offset <- e1@offset + e2@offset
        return(e1)
})
