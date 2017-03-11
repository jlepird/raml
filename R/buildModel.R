#' An object containing all data necessary for preference elicitation.
#' @name RAML
#' @import methods
#' @field sense Should the objective be minimized ("min") or maximized ("max")?
#'
#'
Model <- setRefClass("RAMLClass",
                     fields = c("sense",
                                "objective",
                                "constraints",
                                "variables"),
                     methods = list(
                       initialize = function(...){

                         # Provide default values
                         sense <<- "min"
                         objective <<- NA
                         constraints <<- NA
                         variables <<- NA

                         # Call super to override any defaults
                         callSuper(...)
                       },
                       show = function(){},
                       var = function(defn, integer = "Real"){
                         defn <- match.call()$defn
                         bounds <- c(-Inf, Inf)

                         # First, figure out if the user defined var bounds.
                         equality <- as.character(defn[1])

                         if (equality %in% c(">=", "<=")){
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

                         print(length(defn))
                         if (length(defn) > 1){
                           if (as.character(defn[[1]]) == "[") { # then it's an array
                             stop("Not yet implemented")
                           } else {
                             stop(paste("Unknown error parsing", as.character(defn)))
                           }
                         } else {
                             name <- as.character(defn)
                             return(.defVar(name, bounds, integer))
                           }
                       },
                       constraint = function(){},
                       objective = function(){}
                      )
)

.defVar <- function(name, bounds, integer){
  out <- list(name = name,
              bounds = bounds,
              integer = integer,
              value = NA)
  class(out) <- c("list", "ramlVariable")
}

