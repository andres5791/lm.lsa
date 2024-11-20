#===============================================================================
## demean.data
## Author: Andrés Strello
## Description
## De-mean a dataframe from a set of variables
## Thought to be used for FE effects analyses
## Return: de-meaned dataframe
## Last modified: 2024-11-15
## Dependencies: dplyr
#===============================================================================

#' De-mean data
#'
#' @import dplyr
#'
demean.data <- function(dat,
                        vars,
                        fevar,
                        wgt,
                        allvars = TRUE,
                        keep=""){
  error.if.factor(formula)
  if(!require(dplyr)) stop("Missing dependency: dplyr")

  # Pass to factor the fixed effect variables
  for(i in 1:length(fevar)) dat[[fevar[i]]] <- as.factor(dat[[fevar[i]]])

  # Remove NA cases (we want the variables to be demeaned from the same sample)
  dat.clean <- dat[complete.cases(dat[,c(vars,fevar,wgt)]),]

  # List of de-meaned columns
  list.residuals <- lapply(as.list(vars), function(var){
    dat.clean <- dat.clean %>%
      group_by( !!!syms(fevar) ) %>%
      mutate(TMP.VAR.MEAN = weighted.mean( !!ensym(var) , !!ensym(wgt) ,na.rm=T))

    themean <- weighted.mean(dat.clean[[var]],dat.clean[[wgt]])
    dat.clean$residuals <- dat.clean[[var]] - dat.clean$TMP.VAR.MEAN + themean

    ## Copy attributes
    attributes(dat.clean$residuals) <- attributes(dat.clean[[var]])
    df <-  dat.clean[,"residuals"]
    names(df) <- var
    df
  })

  dat.clean2 <- dat.clean

  # Restore to the dataset the demeaned values
  for(i in 1:length(list.residuals)){
    dat.clean2[[vars[i]]] <- list.residuals[[i]][[1]]
  }

  if(allvars){
    dat.clean2
  } else {
    dat.clean2[c(keep,fevar,wgt,vars)]
  }
}
