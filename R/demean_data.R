#===============================================================================
## demean.data
## Description
## De-mean a dataframe from a set of variables
## Thought to be used for FE effects analyses
## Return: de-meaned dataframe
#===============================================================================

demean.data <- function(data,
                        vars,
                        fevar,
                        wgt,
                        allvars = TRUE,
                        keep=NULL){
  # Pass to factor the fixed effect variables
  for(i in 1:length(fevar)) data[[fevar[i]]] <- as.factor(data[[fevar[i]]])

  # Remove NA cases (we want the variables to be demeaned from the same sample)
  dat.clean <- data[stats::complete.cases(data[,c(vars,fevar,wgt)]),]

  # List of de-meaned columns
  list.residuals <- lapply(as.list(vars), function(var){

    dat.clean[[var]] <- as.numeric(dat.clean[[var]])

    dat.clean <- dat.clean %>%
      dplyr::group_by( !!!syms(fevar) ) %>%
      dplyr::mutate(TMP.VAR.MEAN = stats::weighted.mean(!!dplyr::ensym(var),
                                                        !!dplyr::ensym(wgt),
                                                        na.rm=T))

    themean <- stats::weighted.mean(dat.clean[[var]],dat.clean[[wgt]])
    dat.clean$residuals <- dat.clean[[var]] - dat.clean$TMP.VAR.MEAN + themean

    ## Copy attributes
    attributes(dat.clean$residuals) <- attributes(dat.clean[[var]])
    df <-  dat.clean[,"residuals"]
    names(df) <- var
    df
  })

  # Restore to the dataset the demeaned values
  for(i in 1:length(list.residuals)){
    dat.clean[[vars[i]]] <- list.residuals[[i]][[1]]
  }

  if(allvars){
    dat.clean
  } else {
    dat.clean[c(keep,fevar,wgt,vars)]
  }
}
