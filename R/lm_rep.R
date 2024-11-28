#' Title
#'
#' @param formula model formula
#' @param data a data frame
#' @param wgt a string indicating the column in the data with the total weights
#' @param fevar a string indicating the column(s) in the data with the variables
#' used as fixed-effects. Supports 1 or more. If NULL, then there is no fixed-effect adjustment
#' @param rwgts a vector of strings indicating the names of the replicate weights variables
#'  (e.g., if 150 replicates are wanted, then the vector must be length 150)
#' @param study a string indicating study, supports: 'TIMSS', 'PIRLS','ICILS',"ICCS', and 'PISA'
#' @param ncores indicate desired number of cores used for processing.
#'  1 for no parallel processing.
#'  If NULL (default), the estimation will use half of the logical cores available
#' @param benchmark TRUE if want to estimate the time used per each plausible value estimation
#' @param asList if FALSE (default), returns summary() of the regression
#' if TRUE, return a list with some original lm() elements (such as the dataframe)
#'
#' @return a summary.lm() object
#' @export
#'
#' @import tictoc parallel
#' @importFrom stats pt
#'
#' @examples
#'
#' # Example without fixed-effects, dependent variable 1st plausible value
#' library(dplyr)
#' data.fin.2011 <- mini_pirls[mini_pirls$YEAR %in% "2011" &
#'                             mini_pirls$IDCNTRY %in% 246,]
#' example1 <- lm.rep(ASRREA01 ~ 1 + PRESCH, # formula
#'                       data=data.fin.2011, # example data
#'                       wgt="SENWGT", # weight: senate weights
#'                       fevar=NULL, # no fixed-effects
#'                       rwgts=paste0("RWGT",1:150), # name of replicate weights
#'                       ncores=2,
#'                       study="PIRLS"
#'                       )
#' print(example1)
#'
#'
#' # Example with time and country fixed-effects, dependent variable 1st plausible value
#' example2 <- lm.rep(ASRREA01 ~ 1 + PRESCH, # formula
#'                       data=mini_pirls, # example data
#'                       wgt="SENWGT", # weight: senate weights
#'                       fevar=c("YEAR","IDCNTRY"), # fixed-effects variables
#'                       rwgts=paste0("RWGT",1:150), # name of replicate weights
#'                       ncores=2,
#'                       study="PIRLS"
#'                       )
#' print(example2)
#'
#'
lm.rep <- function(
    formula=y~x,
    data,
    wgt,
    fevar=NULL,
    rwgts,
    study,
    ncores=NULL,
    benchmark=TRUE,
    asList=FALSE
){
  # Capture variables
  depvar <- as.character(formula[[2]])
    if(length(depvar)>1) stop("Only one dependent variable supported")
  indvar <- attr(stats::terms(formula), "term.labels")

  # Calculate original replicate
  if(is.null(fevar)){
    data$TMP_VAR_WEIGHT <- data[[wgt]]

    r0 <- stats::lm(formula,
             data=data,
             weights=TMP_VAR_WEIGHT)
  } else {
    r0 <- lm.fe(formula,
                data=data,
                wgt=wgt,
                fevar=fevar)
  }

  # Determine number of cores if not indicated
  # put half of cores rounding down
  if(is.null(ncores)) ncores <- floor(detectCores()/2)

  # Estimate replicated weights
  if(ncores %in% c(0,1,NA)){
    if(benchmark) tictoc::tic("No parallel processing (cores available = 1)")

    # Remove the tibble characteristics to avoid errors
    # In addition, select only needed columns to use less RAM

   nointeraction <- lapply(as.list(indvar), function(x){
                            unlist(strsplit(x,":"))
                            })
    indvar.clean <- unique(unlist(nointeraction))

      
    mat <- .untidy(data)[,c(fevar,depvar,indvar.clean,wgt,rwgts)]


    # Calculate replicates per weight
    regs <- lapply(as.list(rwgts), function(wgt){
      if(is.null(fevar)){
        mat$TMP_VAR_WEIGHT <- mat[[wgt]]
        stats::lm(formula,
           data=mat,
           weights=TMP_VAR_WEIGHT)
      } else {
        lm.fe(formula,
              data=mat,
              wgt=wgt,
              fevar=fevar
        )
      }
    })
    if(benchmark) tictoc::toc()
  } else {
    # Calculate replicates per weight, parallel
    message("Number of cores used: ",ncores)

    if(benchmark) tictoc::tic("With parallel processing")
    # Remove the tibble characteristics to avoid errors
    # In addition, select only needed columns to use less RAM
    mat <- .untidy(data)[,c(fevar,depvar,indvar,wgt,rwgts)]

    cl <- parallel::makeCluster(ncores)

    parallel::clusterExport(cl, list("lm.fe",
                           "demean.data",
                           "formula",
                           "mat",
                           "fevar",
                           "rwgts",
                           "error.if.factor"),
                  envir=environment())

    regs <- parallel::parLapply(cl, as.list(rwgts), function(wgt) {

      if(is.null(fevar)){
        mat$TMP_VAR_WEIGHT <- mat[[wgt]]
        stats::lm(formula,
           data=mat,
           weights=TMP_VAR_WEIGHT)
      } else {
        lm.fe(formula,
              data = mat,
              wgt = wgt,
              fevar = fevar)
      }
    })
    parallel::stopCluster(cl)
    if(benchmark) tictoc::toc()
  }

  ## Calculate sampling variance
  coef0 <- t(r0$coefficients)

  coefdif <- lapply(1:length(regs), function(i){
    coefr <- t(regs[[i]]$coefficients)
    (coefr-coef0)^2
  })

  ## Some additional checks
  # Error if the number of dimensions are different
  tmp.dim <- length(coefdif[[1]])
  for(i in 2:length(coefdif)) if(length(coefdif[[i]]) != tmp.dim) stop("Different number of dimensions in replication number ", i)

  ## Sum of differences
  mat.coefdif <- matrix(unlist(coefdif),ncol=length(regs),byrow=F)
  rownames(mat.coefdif) <- colnames(coefdif[[1]])

  # The calculations depends on the study
  if(study %in% c("TIMSS","PIRLS")){
    rep.var <-  rowSums(mat.coefdif)/2
  }
  if(study %in% c("ICILS","ICCS")) {
    rep.var <-  rowSums(mat.coefdif)
  }
  if(study %in% c("PISA")) {
    rep.var <-  rowSums(mat.coefdif)/(length(regs)*(1-0.5)^2)
  }

  ## Pass the new standard errors to the lm() object
  reg <- r0

  # Change function call
  reg$nrep <- length(regs)
  reg$call$nrep <- round(as.numeric(length(regs)),0)
  reg$call[[1]] <- as.name("lm.rep")
  names(reg$call)[which(names(reg$call) %in% "weights")] <- "wgt"
  reg$call$wgt <- wgt

  sum_reg <- summary(reg)

  # Calculate the standard error, t-value and p-values
  sum_reg$coefficients[,2] <- sqrt(rep.var)
  sum_reg$coefficients[,3] <- sum_reg$coefficients[,1]/sum_reg$coefficients[,2]
  sum_reg$coefficients[,4] <- 2*pt(-abs(sum_reg$coefficients[,3]),
                                   df=sum_reg$df[2])

  # Correct the F-statistic
  df_model <- sum_reg$fstatistic[["numdf"]] # Degrees of freedom for the model
  df_residual <- sum_reg$fstatistic[["dendf"]] # Residual degrees of freedom
  rss <- sum(stats::residuals(reg)^2)                     # Residual sum of squares
  tss <- sum((stats::model.response(
    stats::model.frame(reg)) - mean(stats::model.response(stats::model.frame(reg))))^2) # Total sum of squares
  ess <- tss - rss                                 # Explained sum of squares (ESS)
  msr <- ess / df_model # Mean square for regression
  mse <- rss / df_residual # Mean square error (residuals)
  f_stat <- msr / mse # F-statistic

  sum_reg$fstatistic <- c(value = f_stat, numdf = df_model, dendf = df_residual)
  sum_reg$fstatistic_pval <- stats::pf(f_stat, df_model, df_residual, lower.tail = FALSE)

  # Update the residual standard error
  sum_reg$sigma <- sqrt(rss/df_residual)

  # Possibility of returning the original values of the lm() object
  # in case we need to further manipulate it
  if(asList){
    theList <- list(
      sum.lm=sum_reg,
      model_matrix=stats::model.matrix(reg),
      model_frame=stats::model.frame(reg),
      residuals=stats::residuals(reg)
    )
    return(theList)
  }
  # If not, return just the summary
  sum_reg
}

