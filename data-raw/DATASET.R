## code to prepare `DATASET` dataset goes here

## Function to create example data

## Preamble ==================================================================
# clean
rm(list = ls()) # clean enviroment

if(!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr,haven)


# Merge data (do only once)
if(FALSE){
  if(!require(ILSAmerge)) install.packages("ILSAmerge")

  indir <- "D:/RandA Dropbox/Data/raw" # %YOUR FOLDER HERE%

  outdir <- "D:/RandA Dropbox/Data/merged"  # %OUTPUT FOLDER HERE%

  ILSAmerge::ILSAmerge( file.path(indir,"TIMSS&PIRLS2011_IDB_SPSS","Data"),
                          file.path(outdir,"TIMSS_PIRLS_2011"))

  ILSAmerge::ILSAmerge( file.path(indir,"PIRLS2001_IDB_SPSS","Data"),
                        file.path(outdir,"PIRLS2001"))

  ILSAmerge::ILSAmerge( file.path(indir,"PIRLS2006_IDB_SPSS","Data"),
                        file.path(outdir,"PIRLS2006"))

  ILSAmerge::ILSAmerge( file.path(indir,"PIRLS2016_IDB_SPSS","Data"),
                        file.path(outdir,"PIRLS2016"))
}

## Names folders: NAME FOLDER = year, code, presch, lang, gender
list.folders <- list("PIRLS2001"=c("2001","R1","ASBH0TIM","ASBGLANH","ITSEX"),
                     "PIRLS2006"=c("2006","R2","ASBH0HLO","ASBGLNGH","ITSEX"),
                     "TIMSS_PIRLS_2011"=c("2011","B1","ASDHAPS","ASBG03","ITSEX"),
                     "PIRLS2016"=c("2016","R4","ASDHAPS","ASBG03","ITSEX"))

## Set folder
outdir <- "D:/RandA Dropbox/Data/merged" # %outdir folder%

# Read all datas and do some cleaning
l.data <- lapply(1:length(list.folders), function(i){
  dat1 <- readRDS(file.path(
    outdir,names(list.folders)[i],
    paste0("ASG",list.folders[[i]][2],".rds")))

  dat2 <- readRDS(file.path(
    outdir,names(list.folders)[i],
    paste0("ASH",list.folders[[i]][2],".rds")))

  tmp.tokeep <- c("IDCNTRY","IDSTUD",
                  names(dat2)[!names(dat2) %in% names(dat1)])

  dat <- left_join(dat1,dat2[tmp.tokeep],join_by(IDCNTRY,IDSTUD))

  rm(dat1,dat2)
  gc()

  # Achievement variables
  achvars <- c(paste0("ASRREA0",1:5),paste0("ASMMAT0",1:5))
  if(list.folders[[i]][1] %in% c("2001","2006","2016")){
    for(k in 1:5) dat[[paste0("ASMMAT0",k)]] <- as.numeric(NA)
  }

  # Add early literacy and numeracy activities in 2011, for example on student FE
  dat$EARLYLIT <- NA
  dat$EARLYNUM <- NA
  if(list.folders[[i]][1] %in% c("2011")){
    dat$EARLYLIT <- dat$ASBHELA
    dat$EARLYNUM <- dat$ASBHENA
      dat[dat$ASBHELA %in% c(999996,999999),"EARLYLIT"] <- NA
      dat[dat$ASBHENA %in% c(999996,999999),"EARLYNUM"] <- NA
  }
  dat$PRESCH <- dat[[list.folders[[i]][3]]]
  dat$LANG <- dat[[list.folders[[i]][4]]]
  dat$GIRL <- dat[[list.folders[[i]][5]]]

  dat$YEAR <- list.folders[[i]][1]



  # Transformations by year

  if(list.folders[[i]][1] %in% c("2001","2006","2011")){
    dat[dat[[list.folders[[i]][4]]] %in% c(1,2),"LANG"] <- 0
    dat[dat[[list.folders[[i]][4]]] %in% 3,"LANG"] <- 1

    attr(dat$LANG,"labels") <- c(
      "Sometimes or always speak language at home"=0,
      "Never speak language at home"=1,
      "Omitted or invalid"=9
    )
  }

  if(list.folders[[i]][1] %in% c("2001")){
    dat[dat[[list.folders[[i]][3]]] %in% c(4,5,6),"PRESCH"] <- 0
    dat[dat[[list.folders[[i]][3]]] %in% c(1,2,3),"PRESCH"] <- 1

    attr(dat$PRESCH,"labels") <- c(
      "Did not attend or attended 1 year or less"=0,
      "Attended more than 1 year"=1,
      "Omitted or invalid"=9
    )
  }

  if(list.folders[[i]][1] %in% c("2006")){
    dat[dat[[list.folders[[i]][3]]] %in% c(5,6),"PRESCH"] <- 0
    dat[dat[[list.folders[[i]][3]]] %in% c(1,2,3,4),"PRESCH"] <- 1

    attr(dat$PRESCH,"labels") <- c(
      "Did not attend or attended 1 year or less"=0,
      "Attended more than 1 year"=1,
      "Omitted or invalid"=9
    )
  }


  if(list.folders[[i]][1] %in% c("2011")){
    dat[dat[[list.folders[[i]][3]]] %in% c(3,4,6),"PRESCH"] <- 0
    dat[dat[[list.folders[[i]][3]]] %in% c(1,2),"PRESCH"] <- 1

    attr(dat$PRESCH,"labels") <- c(
      "Did not attend or attended 1 year or less"=0,
      "Attended more than 1 year"=1,
      "Omitted or invalid"=9
    )
  }

  if(list.folders[[i]][1] %in% c("2016")){
    dat[dat[[list.folders[[i]][3]]] %in% c(0,1),"PRESCH"] <- 0
    dat[dat[[list.folders[[i]][3]]] %in% c(2,3),"PRESCH"] <- 1

    attr(dat$PRESCH,"labels") <- c(
      "Did not attend or attended 1 year or less"=0,
      "Attended more than 1 year"=1,
      "Omitted or invalid"=9
    )

    dat[dat[[list.folders[[i]][4]]] %in% c(1,2,3),"LANG"] <- 0
    dat[dat[[list.folders[[i]][4]]] %in% 4,"LANG"] <- 1

    attr(dat$LANG,"labels") <- c(
      "Sometimes or always speak language at home"=0,
      "Never speak language at home"=1,
      "Omitted or invalid"=9
    )
  }

  dat[dat[[list.folders[[i]][5]]] %in% 2,"GIRL"] <- 0
  dat[dat[[list.folders[[i]][5]]] %in% 1,"GIRL"] <- 1

  attr(dat$GIRL,"labels") <- c(
    "Boy"=0,
    "Girl"=1,
    "Omitted or invalid"=9
  )

  dat[dat$LANG %in% 9,"LANG"] <- NA
  dat[dat$PRESCH %in% 9,"PRESCH"] <- NA
  dat[dat$GIRL %in% 9,"GIRL"] <- NA

  dat[,c("YEAR","IDCNTRY","IDSCHOOL","IDSTUD","TOTWGT","JKZONE","JKREP",
         "PRESCH","LANG","GIRL",achvars)]
})

# Select countries with more than 1 participation
select.cnts.l <- unlist(lapply(l.data, function(x){
  unique(x$IDCNTRY)
}))
tmp <- table(select.cnts.l)
select.cnts <-  as.numeric(names(tmp[tmp>1]))


l.reduced.data <- lapply(l.data, function(data){
  # keep countries with MORE than 1 participation
  data <- data[data$IDCNTRY %in% select.cnts,]

  # remove missing cases
  data <- data[complete.cases(data[c("LANG","PRESCH","GIRL")]),]

  # sample 20 schools per country
  reduced.cnt <- lapply(unique(data$IDCNTRY), function(cnt){
    mat <- data[data$IDCNTRY %in% cnt,]
    schools <- unique(mat$IDSCHOOL)
    schools_s <- sample(schools,size = 20,replace = FALSE)
    mat[mat$IDSCHOOL %in% schools_s,]
  })

  # generate senate weights
  reduced.cnt <- lapply(reduced.cnt, function(mat){
    mat$SENWGT <- khipu::senate(wt=mat$TOTWGT,
                                group=c(mat$IDCNTRY),
                                N=200)

    # generate senate weights
    repwgt <- khipu::repcreate(df=khipu::untibble(mat),
                               wt="SENWGT",
                               jkzone="JKZONE",
                               jkrep="JKREP",
                               repwtname="RWGT",
                               reps=75,
                               method="TIMSS")
    mat <- bind_cols(mat,repwgt)

    mat$TOTWGT <- NULL

    mat
  })

  data <- do.call(bind_rows,reduced.cnt)
})

mini_pirls <- do.call(bind_rows,l.reduced.data)

usethis::use_data(mini_pirls, overwrite = TRUE)
