tolong.example <- function(x){
  l.dat <- list(
    READ = x,
    MATH = x
  )

  names.domain <- c("READ","MATH")
  names.vars <- c("ASRREA0","ASMMAT0")
  names.early <- c("EARLYLIT","EARLYNUM")
  l.dat <- lapply(as.list(1:2), function(i){
    for(k in 1:5){
      l.dat[[i]][[paste0("ACHIEVEMENT",k)]] <- l.dat[[i]][[paste0(names.vars[i],k)]]
    }
    l.dat[[i]][paste0(rep(names.vars,5),1:5)] <- NULL

    l.dat[[i]]$ACTIVITIES <- l.dat[[i]][[paste0(names.early[i])]]
    l.dat[[i]][names.early] <- NULL

    l.dat[[i]]$DOMAIN <- names.domain[i]

    l.dat[[i]]
  })

  do.call(dplyr::bind_rows,l.dat)
}
