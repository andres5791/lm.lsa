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
#' @import dplyr tibble
#' @importFrom magrittr %>%
#' @importFrom stats pt
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
  # error.if.factor(formula)

  # Weights of 1 if no weight is specified
  if(is.null(wgt)){
    data$TMP.VAR.WEIGHT <- 1
    wgt <- "TMP.VAR.WEIGHT"
  }

  # Transform the data to recognize factor and interactions
  nofact <- recognize.factor.interaction(data = .untidy(data),
                                         formula = formula,
                                         wgt = wgt,
                                         fevar = fevar)

  # Find the relevant variables
  depvar <- as.character(nofact$formula[[2]])
  indvar <- nofact$indvar


  # De-mean the data
  de.dat <- demean.data(nofact$data,
                        vars=c(depvar,indvar),
                        fevar=fevar,
                        wgt=wgt)

  # Remove the tibble characteristics to avoid errors
  if(tibble::is_tibble(de.dat)) de.dat <- .untidy(de.dat)

  # For some reason, this step is needed when running lm() within a function
  de.dat[["TMP.VAR.WEIGHT"]] <- de.dat[[wgt]]

  # Run the regression
  reg <- stats::lm(formula=nofact$formula,
            data=de.dat,
            weights=TMP.VAR.WEIGHT)

  # Add properties
  reg$call$weights <- substitute(wgt)
  reg$call$data <- substitute(data)
  reg$call$fevar <- fevar
  reg$fixed.effects <- paste("Fixed effects: ", paste0(fevar, collapse = " "))
  reg$call[[1]] <- as.name("lm.fe")
  names(reg$call)[which(names(reg$call) %in% "weights")] <- "wgt"
  reg$call$formula <- formula

  # Return
  reg
}

