#' An object containing all data necessary for preference elicitation.
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

#' Defines a variable in the current scope
.defArray <- function(name, bounds, integer, indicies, indexDisplay){
  all.indicies <- expand.grid(indicies)
  all.indicies$index <- apply(all.indicies, 1, function(x) paste0("_", paste(x, collapse = "_")))
  out <- list(name = name,
              bounds = bounds,
              integer = integer,
              indicies = all.indicies$index,
              indexDisplay = indexDisplay,
              value = NA)
  class(out) <- c("list", "ramlArray")

  for (i in 1:nrow(all.indicies)){
    # raml:::.defVar(paste0(name, all.indicies$index[i], bounds, integer))
    name2 <- paste0(name, all.indicies$index[i])
    txt <- paste0(name2, " <- raml:::.defVar(\"", name2 , "\", c(", bounds[1], ",", bounds[2], ")", ",\"", integer, "\")")
    eval.parent(parse(text = txt))
  }
  return(out)
}

.defVar <- function(name, bounds, integer){
  out <- list(name = name,
              bounds = bounds,
              integer = integer,
              value = NA)
  class(out) <- c("list", "ramlVariable")
  return(out)
}

#' Nice display for single variables.
#' @export
#' @param x The variable to be printed, of the "ramlVariable" class.
#' @param ... Other arguments to pass on to print.
print.ramlVariable <- function(x, ...){
  lbBracket <- "["
  if (x$bounds[1] == -Inf) {
    lbBracket <- "("
  }
  ubBracket <- "]"
  if (x$bounds[2] == Inf) {
    ubBracket <- ")"
  }
  if (x$integer == "Real") {
    field <- "\U211D"
  } else if (x$integer == "Integer") {
    field <- "\U2124"
  } else if (x$integer == "Binary"){
    field <- "{0, 1}"
  }

  if (sum(x$bounds == c(-Inf, Inf)) == 2) {
    boundStatement <- ""
  } else {
    boundStatement <- paste0(" \U2229 ",lbBracket, x$bounds[1],", ", x$bounds[2], ubBracket)
  }

  cat(paste0(x$name, " \U2208 ", field, boundStatement), ...)
}

#' Nice display for arrays.
#' @export
#' @param x The variable to be printed, of the "ramlArray" class.
#' @param ... Other arguments to pass on to print.
print.ramlArray <- function(x, ...){
  x$name <- paste0(x$name, "[i]")
  print.ramlVariable(x)
  cat(paste(" \U2200 i \U2208", x$indexDisplay))
}
