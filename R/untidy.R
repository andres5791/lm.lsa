# Remove the tibble characteristics to avoid errors
# Function .untidy from tucuyricuy package
# Since tucuyricuy is private package, added here
# with permission from Andrés Christiansen

.untidy <- function(x){
  out <- x
  out <- lapply(1:ncol(x),function(X){as.vector(out[,X,drop = TRUE])})
  out <- do.call(cbind.data.frame,out)
  colnames(out) <- colnames(x)
  out
  }
