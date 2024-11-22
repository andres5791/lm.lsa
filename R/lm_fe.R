#===============================================================================
## lm.fe
## Author: Andrés Strello
## Description
## Run lm() on a de-meaned dataframe (fixed effects)
## Return: lm() object
## Last modified: 2024-11-15
#===============================================================================

#' Linear regression with fixed-effects
#'
#' Run regression analyses with fixed-effects.
#' Does not support factor() and interactions yet.
#'
#' @param formula model formula
#' @param data a dataframe
#' @param wgt a string indicating the column in the data with the total weights
#' @param fevar a string indicating the column(s) in the data with the variables
#' used as fixed-effects. Supports 1 or more. If NULL, then there is no fixed-effect adjustment
#' @param ... additional options passed to lm(), not working right now
#'
#' @return A lm() object
#' @export
#'
#' @examples
#'
#' example.fe <- lm.fe(ASMMAT01 ~ 1 + PRESCH + LANG,
#'                     data = mini_pirls,
#'                     wgt = "SENWGT",
#'                     fevar = c("YEAR","IDCNTRY")
#'                     )
#' summary(example.fe)
#'
lm.fe <- function(formula = y ~ x,
                  data,
                  wgt=NULL,
                  fevar,
                  ...){
  error.if.factor(formula)
  depvar <- as.character(formula[[2]])
  indvar <- attr(terms(formula), "term.labels")

  # Weights of 1 if no weight is specified
  if(is.null(wgt)){
    data$TMP.VAR.WEIGHT <- 1
    wgt <- "TMP.VAR.WEIGHT"
  }

  # De-mean the data
  de.dat <- demean.data(data,
                        vars=c(depvar,indvar),
                        fevar=fevar,
                        wgt=wgt)

  # Remove the tibble characteristics to avoid errors
  # Function .untidy from tucuyricuy package
  # added here with permission from Andrés Christiansen
  .untidy <- function(x){
    out <- x
    out <- lapply(1:ncol(x),function(X){as.vector(out[,X,drop = TRUE])})
    out <- do.call(cbind.data.frame,out)
    colnames(out) <- colnames(x)
    out }
  de.dat <- .untidy(de.dat)

  # For some reason, this step is needed when running lm() within a function
  de.dat[["TMP.VAR.WEIGHT"]] <- de.dat[[wgt]]


  # Run the regression
  reg <- lm(formula=formula,
            data=de.dat,
            weights=TMP.VAR.WEIGHT,
            ...)

  # Add properties
  reg$call$weights <- substitute(wgt)
  reg$call$data <- substitute(data)
  reg$call$fevar <- fevar
  reg$fixed.effects <- paste("Fixed effects: ", paste0(fevar, collapse = " "))
  reg$call[[1]] <- as.name("lm.fe")
  names(reg$call)[which(names(reg$call) %in% "weights")] <- "wgt"

  # Return
  reg
}

