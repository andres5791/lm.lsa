#===============================================================================
## lm.rep
## Author: Andrés Strello
## Description
## Run lm() using replicate weights, supports lm.fe() and lm()
## Return: summary(lm()) object
## dependencies: dplyr,khipu
## Last modified: 2024-11-18
#===============================================================================

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
  error.if.factor(formula)
  
  # Capture variables
  depvar <- as.character(formula[[2]])
    if(length(depvar)>1) stop("Only one dependent variable supported")
  indvar <- attr(terms(formula), "term.labels")
  
  # Calculate original replicate
  if(is.null(fevar)){
    data$TMP_VAR_WEIGHT <- data[[wgt]]
    
    r0 <- lm(formula,
             data=data,
             weights=TMP_VAR_WEIGHT)
  } else {
    r0 <- lm.fe(formula,
                data=data,
                wgt=wgt,
                fevar=fevar)
  }
  
  # Determine number of cores if not indicated
  if(is.null(ncores)) ncores <- floor(detectCores()/2)
  
  # Estimate replicated weights
  if(ncores %in% c(0,1,NA)){
    if(benchmark) tic("No parallel processing (cores available = 1)")
    
    # Remove the tibble characteristics to avoid errors
    # Function .untidy from tucuyricuy package
    # added here with permission from Andrés Christiansen
    .untidy <- function(x){
      out <- x
      out <- lapply(1:ncol(x),function(X){as.vector(out[,X,drop = TRUE])})
      out <- do.call(cbind.data.frame,out)   
      colnames(out) <- colnames(x)   
      out }
    # In addition, select only needed columns to use less RAM
    mat <- .untidy(data)[,c(fevar,depvar,indvar,wgt,rwgts)]
    
    
    # Calculate replicates per weight
    regs <- lapply(as.list(rwgts), function(wgt){
      #message(wgt," ",appendLF = F)
      
      if(is.null(fevar)){
        mat$TMP_VAR_WEIGHT <- mat[[wgt]]
        lm(formula,
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
    if(benchmark) toc()
  } else {
    # Calculate replicates per weight, parallel
    message("Number of cores used: ",ncores)
    
    if(benchmark) tic("With parallel processing")
    # Remove the tibble characteristics to avoid errors
    # Function .untidy from tucuyricuy package
    # added here with permission from Andrés Christiansen
    .untidy <- function(x){
      out <- x
      out <- lapply(1:ncol(x),function(X){as.vector(out[,X,drop = TRUE])})
      out <- do.call(cbind.data.frame,out)   
      colnames(out) <- colnames(x)   
      out }
    # In addition, select only needed columns to use less RAM
    mat <- .untidy(data)[,c(fevar,depvar,indvar,wgt,rwgts)]
    
    cl <- makeCluster(ncores)
    
    clusterExport(cl, list("lm.fe",
                           "demean.data",
                           "formula",
                           "mat",
                           "fevar",
                           "rwgts",
                           "error.if.factor"),
                  envir=environment())
    
    regs <- parLapply(cl, as.list(rwgts), function(wgt) {

      if(is.null(fevar)){
        browser()
        mat$TMP_VAR_WEIGHT <- mat[[wgt]]
        lm(formula,
           data=mat,
           weights=TMP_VAR_WEIGHT)
      } else {
        lm.fe(formula,
              data = mat,
              wgt = wgt,
              fevar = fevar)
      }
    })
    stopCluster(cl)
    if(benchmark) toc()
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
  
  sum_reg <- summary(reg)
  
  browser()
  
  # Calculate the standard error, t-value and p-values
  sum_reg$coefficients[,2] <- sqrt(rep.var)
  sum_reg$coefficients[,3] <- sum_reg$coefficients[,1]/sum_reg$coefficients[,2]
  sum_reg$coefficients[,4] <- 2*pt(-abs(sum_reg$coefficients[,3]),
                                   df=sum_reg$df[2]) 
  
  # Correct the F-statistic
  df_model <- sum_reg$fstatistic[["numdf"]] # Degrees of freedom for the model
  df_residual <- sum_reg$fstatistic[["dendf"]] # Residual degrees of freedom
  rss <- sum(residuals(reg)^2)                     # Residual sum of squares
  tss <- sum((model.response(model.frame(reg)) - mean(model.response(model.frame(reg))))^2) # Total sum of squares
  ess <- tss - rss                                 # Explained sum of squares (ESS)
  msr <- ess / df_model # Mean square for regression
  mse <- rss / df_residual # Mean square error (residuals)
  f_stat <- msr / mse # F-statistic
  
  sum_reg$fstatistic <- c(value = f_stat, numdf = df_model, dendf = df_residual)
  sum_reg$fstatistic_pval <- pf(f_stat, df_model, df_residual, lower.tail = FALSE)
  
  # Update the residual standard error
  sum_reg$sigma <- sqrt(rss/df_residual)
  
  # Possibility of returning the original values of the lm() object
  # in case we need to further manipulate it
  if(asList){
    theList <- list(
      sum.lm=sum_reg,
      model_matrix=model.matrix(reg),
      model_frame=model.frame(reg),
      residuals=residuals(reg)
    )
    return(theList)
  }
  # If not, return just the summary
  sum_reg
}

