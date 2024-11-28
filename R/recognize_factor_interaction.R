recognize.factor.interaction <- function(data,
                                         formula,
                                         wgt=wgt,
                                         fevar=fevar){
## BLOCK A: IDENTIFY VARIABLES =================================================

depvar <- as.character(formula[[2]])
indvar <- attr(stats::terms(formula), "term.labels")


# Remove interactions
nointeraction <- lapply(as.list(indvar), function(x){
  unlist(strsplit(x,":"))
})

indvar.clean <- unique(unlist(nointeraction))


# Identify factors via formula or via attributes
any.factor <-  lapply(as.list(indvar.clean), function(x){
  ident.facts.for <- grepl("^factor\\(", x)

  ident.facts.att <- ifelse(is.factor(data[[x]]),TRUE,FALSE)

  ident.facts <- ifelse(any(ident.facts.for,ident.facts.att),
                        TRUE,
                        FALSE)

  no.factors <- gsub("^factor\\((.*)\\)$", "\\1", x)

  list(
    ident.facts=ident.facts,
    no.factors=no.factors)
})


## BLOCK B: Prepare dataset to be de-meaned ====================================

# Change the columns with factors to group of dummies
continuous <-  lapply(any.factor, function(x){
  if(x$ident.facts){

    # Pass the relevant variables to factor
    if(!is.character(data[[x$no.factors]])) {
      var <- as.factor(as.numeric(data[[x$no.factors]]))
    } else {
      var <- as.factor(data[[x$no.factors]])
    }

    # Dichotomize
    le <- levels(as.factor(data[[x$no.factors]]))
    vars <- lapply(as.list(le), function(i){
      var2 <- ifelse(var %in% i, 1, 0)
      var2 <- ifelse(is.na(var), NA, var2)
    })

    names(vars) <-  sapply(le, function(i) paste0(x$no.factors,i),
                           USE.NAMES=FALSE)

    # Avoid that the new names overlap with other variable in the data
    while((any(names(vars) %in% names(data)))){
      names(vars) <- paste0(names(vars),"_")
    }

    # Delete reference value
    vars[[1]] <- NULL

    # Combine into dataset
    mat <- do.call(dplyr::bind_cols,vars)
  } else {
    data[x$no.factors]
    }
})

# Combine with dataset
data2 <- cbind(data[c(depvar,wgt,fevar)],
               do.call(dplyr::bind_cols,continuous))
rm(continuous)
gc()


# BLOCK C: ADAPT FORMULA =======================================================

# List the names that are factors
factor.vars <- unique(unlist(sapply(any.factor, function(x) {
  if(x$ident.facts){
    x$no.factors
  }
})))

# Remove factors from formula
indvar.clean2 <- gsub("factor\\(([^\\)]+)\\)", "\\1", indvar)

# Change all factor variables to the variables in the new dataset
indvar.def <- indvar.clean2
i <- 1

if(length(factor.vars) > 0){
  for(i in 1:length(factor.vars)){
    tmp_old <- factor.vars[i]

    le <- levels(as.factor(data[[tmp_old]]))[-1] # get the levels except the ref

    tmp_new <- sapply(le, function(j) paste0(tmp_old,j), USE.NAMES = FALSE)

    tmp_news <- lapply(as.list(indvar.def), function(j){
      sapply(tmp_new, function(k) gsub(tmp_old,k,j))
    })

    indvar.def <-  unlist(sapply(tmp_news, function(j) unique(j)))
  }
}
# Convert back to formula
formula_new <- stats::as.formula( paste(
  paste(depvar, collapse = " + "),
  as.character(formula[[1]]),
  paste(c(as.character(attributes(stats::terms(formula))$intercept),
    paste0(indvar.def, collapse = " + ")),
    collapse=" + ")))

# To return the independent variables only
indvar.return <- unique(unlist(lapply(as.list(indvar.def), function(x){
  unlist(strsplit(x,":"))
})))

## RETURN ======================================================================

list(
  data = .untidy(data2),
  formula = formula_new,
  indvar = indvar.return
)
}
