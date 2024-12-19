#' Run regression analyses with ILSA data
#'
#' Run regression analyses with replicate weights and combining plausible values.
#' Supports TIMSS, PIRLS, ICILS, ICCS, and PISA.
#' Does not support factor() and interactions yet
#'
#' @param formula model formula
#' @param data a data frame
#' @param wgt a string indicating the column in the data with the total weights
#' @param fevar a string indicating the column(s) in the data with the variables
#' used as fixed-effects. Supports 1 or more. If NULL, then there is no fixed-effect adjustment
#' @param rwgts a vector of strings indicating the names of the replicate weights variables
#'  (e.g., if 150 replicates are wanted, then the vector must be length 150)
#' @param study a string indicating study, supports: 'TIMSS', 'PIRLS','ICILS',"ICCS', and 'PISA'
#' @param pvs a named list.
#' Each list element name indicates how is referred within the formula
#' Each list element must contain a vector of strings indicating the name in the dataset
#' @param ncores indicate desired number of cores used for processing.
#'  1 for no parallel processing.
#'  If NULL (default), the estimation will use half of the logical cores available
#' @param benchmark TRUE if want to estimate the time used per each plausible value estimation
#'
#' @importFrom stats pt
#'
#' @return a summary.lm() object
#' @export
#'
#' @examples
#'
#' # Example without fixed-effects
#' data.fin.2011 <- mini_pirls[mini_pirls$YEAR %in% "2011" &
#'                             mini_pirls$IDCNTRY %in% 246,]
#' example1 <- lm.lsa(ASRREA ~ 1 + PRESCH, # formula
#'                       data=data.fin.2011, # example data
#'                       wgt="SENWGT", # weight: senate weights
#'                       fevar=NULL, # no fixed-effects
#'                       rwgts=paste0("RWGT",1:150), # name of replicate weights
#'                       pvs=list(ASRREA=paste0("ASRREA0",1:5)), # list with PVs variables
#'                       ncores=2,
#'                       study="PIRLS"
#' )
#'print(example1)
#'
#'
#' # Example with time and country fixed-effects
#' example2 <- lm.lsa(ASRREA ~ 1 + PRESCH, # formula
#'                       data=mini_pirls, # example data
#'                       wgt="SENWGT", # weight: senate weights
#'                       fevar=c("YEAR","IDCNTRY"), # fixed-effects variables
#'                       rwgts=paste0("RWGT",1:150), # name of replicate weights
#'                       pvs=list(ASRREA=paste0("ASRREA0",1:5)), # list with PVs variables
#'                       ncores=2,
#'                       study="PIRLS"
#' )
#'print(example2)
#'
#'
#'
lm.lsa <- function(
    formula=y~x,
    data,
    wgt,
    fevar=NULL,
    rwgts,
    study,
    pvs,
    ncores=NULL,
    benchmark=FALSE
){
  ## Preamble ====================================================================
  # Move formula to formula if was a string
  if(is.character(formula)) formula <- stats::as.formula(formula)
  
  # Capture variables
  depvar <- as.character(formula[[2]])
  if(length(depvar)>1) stop("Only one dependent variable supported")
  indvar <- attr(stats::terms(formula), "term.labels")

  # Make sure PVs are correctly specified
  if(!is.list(pvs)) stop("Argument pvs must be a named list")
  if(length(names(pvs))==0) stop("Argument pvs elements must be named")

  # Recognize if PVs is a variable present in the dataset (create error)
  if(any(names(pvs) %in% names(data))) stop(
    "To identify the plausible values, make sure they are not present in dataset")

  # Recognize if PVs are used
  if(all(!is.na(pvs))){
    if(any(c(depvar,indvar) %in% names(pvs))){
      pv_depvar <- names(pvs)[names(pvs) %in% depvar]
      pv_indvar <- names(pvs)[names(pvs) %in% indvar]

      if(any(!names(pvs) %in% c(pv_depvar,pv_indvar))){
        message("Please note PVs specified not present in formula: ",
                paste(names(pvs)[!names(pvs) %in% c(pv_depvar,pv_indvar)],
                      collapse=" "))}
    } else {
      message("Please note PVs specified not present in formula: ",
              paste(names(pvs),collapse=" "))
      pv_depvar <- character(0)
      pv_indvar <- character(0)
    }
  } else {
    pv_depvar <- character(0)
    pv_indvar <- character(0)
  }

  # Identify number of plausible values and make sure they are the same length
  # For now, PVs must be related (no 5x5 replications for example)
  npvs <- as.vector(sapply(pvs, function(i) length(i)))
  if(length(unique(npvs)) > 1) stop("Must be the same number of plausible values",
                                    " in all specified elements.")
  npvs <- npvs[1]

  # Remove the tibble characteristics to avoid errors
  # have a smaller dataset so it takes less RAM
  nointeraction <- lapply(as.list(indvar), 
                          function(x) unlist(strsplit(x,":")))
  nofactor <- gsub("^factor\\((.*)\\)$", "\\1", unlist(nointeraction))
  indvar.clean <- unique(nofactor)

  allvar <- c(indvar.clean,depvar)
  if(any(allvar %in% c(pv_depvar,pv_indvar))){
    to.replace <- allvar[allvar %in% c(pv_depvar,pv_indvar)]
    
    for(i in 1:length(to.replace)){
      detect.str <- to.replace[i]
      allvar <- allvar[!allvar %in% detect.str]
      allvar <- c(allvar, pvs[[detect.str]])
    }
    
  }
  
  mat <- .untidy(data)[,c(fevar,allvar,wgt,rwgts)]
  
  
## Compute the model per plausible value =======================================

# determining number of cores
# Number of cores
if(is.null(ncores)) ncores <- floor(detectCores()/2)
if(ncores > npvs) ncores <- npvs  
  
  
  if(ncores %in% c(0,1,NA)){
    ## One core  version
    sum_regpvs <- lapply(1:npvs, function(i){
      message("Running plausible value ",i," with ",length(rwgts)," replicates")
      # Re-adapt formula with the correct names in the dataset
      new_formula <- formula
      
      if(length(pv_depvar) > 0) { for(k in 1:length(pv_depvar)){
        new_pv_var <- pvs[[pv_depvar[k]]][i]
        new_formula <- stats::as.formula(gsub(pv_depvar[k],
                                              new_pv_var,
                                              deparse(new_formula)))
      }}
      if(length(pv_indvar) > 0) { for(k in 1:length(pv_indvar)){
        new_pv_var <- pvs[[pv_indvar[k]]][i]
        new_formula <- stats::as.formula(gsub(pv_indvar[k],
                                              new_pv_var,
                                              deparse(new_formula)))
      }}
      
      gc()
      
      repmodel <- lm.rep(new_formula,
                         data=mat,
                         wgt=wgt,
                         rwgts=rwgts,
                         fevar=fevar,
                         ncores=ncores,
                         study=study,
                         benchmark=benchmark,
                         asList=FALSE)
      gc() # Make sure we dump the unused RAM before continuing...
      repmodel
    })
  } else {
    ## Parallel version
    message("ncores = ",ncores)
    
    # Open the connection
    cl <- parallel::makeCluster(ncores)
    parallel::clusterExport(cl, list("lm.fe",
                                     "demean.data",
                                     "formula",
                                     "mat",
                                     "fevar",
                                     "rwgts",
                                     "lm.rep",
                                     "recognize.factor.interaction"),
                            envir=environment())    
    
    sum_regpvs <- parallel::parLapply(cl, 1:npvs, function(i){
      # Re-adapt formula with the correct names in the dataset
      new_formula <- formula
      
      if(length(pv_depvar) > 0) { for(k in 1:length(pv_depvar)){
        new_pv_var <- pvs[[pv_depvar[k]]][i]
        new_formula <- stats::as.formula(gsub(pv_depvar[k],
                                              new_pv_var,
                                              deparse(new_formula)))
      }}
      if(length(pv_indvar) > 0) { for(k in 1:length(pv_indvar)){
        new_pv_var <- pvs[[pv_indvar[k]]][i]
        new_formula <- stats::as.formula(gsub(pv_indvar[k],
                                              new_pv_var,
                                              deparse(new_formula)))
      }}
      
      gc()

      repmodel <- lm.rep(new_formula,
                         data=mat,
                         wgt=wgt,
                         rwgts=rwgts,
                         fevar=fevar,
                         study=study,
                         benchmark=benchmark,
                         asList=FALSE,
                         ncores=1)
      gc() # Make sure we dump the unused RAM before continuing...
      repmodel
    })
  }
  
  
  
  ## Calculate estimations
  coef.pvs <- lapply(sum_regpvs, function(x) x$coefficients[,"Estimate"])
  coef.pvs.mat <- matrix(unlist(coef.pvs),nrow=npvs,byrow=T)
  colnames(coef.pvs.mat) <- names(coef.pvs[[1]])
  mean.est <- colMeans(coef.pvs.mat)

  ## Calculate standard errors
  # Calculate sampling variance (mean of the variances)
  se.pvs <- lapply(sum_regpvs, function(x) x$coefficients[,"Std. Error"])
  var.pvs.mat <- matrix(unlist(se.pvs),nrow=npvs,byrow=T)^2
                  #^2 because it's variance !
  colnames(var.pvs.mat) <- names(se.pvs[[1]])
  samp.var <- colMeans(var.pvs.mat)

  # Calculate imputation variance
  mean.est.mat <- matrix(rep(mean.est,npvs),
                         nrow=npvs,byrow=T)
  var.pvs.mat <-  (coef.pvs.mat - mean.est.mat)^2
  imp.var <- colSums(var.pvs.mat)/(npvs-1)

  # Final standard error
  se.model <- sqrt(samp.var + (1+1/npvs)*imp.var)

  ## Create the new lm() object with updated values
  sum_reg <- sum_regpvs[[1]]

  # Update the estimates, standard error, t-value and p-values
  sum_reg$coefficients[,1] <- mean.est
  sum_reg$coefficients[,2] <- se.model
  sum_reg$coefficients[,3] <- sum_reg$coefficients[,1]/sum_reg$coefficients[,2]
  sum_reg$coefficients[,4] <- 2*pt(-abs(sum_reg$coefficients[,3]),
                                   df=sum_reg$df[2])

  sum_reg$r.squared <- mean(unlist(lapply(sum_regpvs,
                                          function(x) x$r.squared)))
  sum_reg$adj.r.squared <- mean(unlist(lapply(sum_regpvs,
                                              function(x) x$adj.r.squared)))

  # Correct the F-statistic
  df_model <- sum_reg$fstatistic[["numdf"]] # Degrees of freedom for the model
  df_residual <- sum_reg$fstatistic[["dendf"]] # Residual degrees of freedom
  f_stat <- mean(unlist(lapply(sum_regpvs,
                               function(x) x$fstatistic[[1]])))
  sum_reg$fstatistic <- c(value = f_stat, numdf = df_model, dendf = df_residual)
  sum_reg$fstatistic_pval <- stats::pf(f_stat, df_model, df_residual, lower.tail = FALSE)

  # Update the residual standard error
  sum_reg$sigma <- mean(unlist(lapply(sum_regpvs, function(x) x$sigma)))

  ## Update the call arguments
  # Update formula
  sum_reg$call$formula <- formula

  # Add number of PVs
  sum_reg$call$npvs <- round(as.numeric(length(pvs[[1]])),0)

  # Change to the name of the function
  sum_reg$call[[1]] <- as.name("lm.lsa")

  # Change name weights
  sum_reg$call$wgt <- wgt

  ## Update the coefficient names to remove the PV number
  rownames(sum_reg$coefficients) <- sapply(rownames(sum_reg$coefficients),
                                           function(x) {
                                             check.name.pvs <- unlist(lapply(pvs,function(y) y[[1]]))
                                             if(any(x %in% check.name.pvs)){
                                               x <- names(check.name.pvs)[which(check.name.pvs %in% x)]
                                             } else {
                                               x
                                             }
                                           })

  gc()
  sum_reg
}
