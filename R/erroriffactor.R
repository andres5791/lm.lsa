## Recognize factors
# Small function as factor(variable) is not supported still 

error.if.factor <- function(formula=y~x){
  formula_str <- deparse(formula)
  has_factor <- grepl("factor\\(", formula_str)
  
  if(any(has_factor)) stop("Formulas with factor() terms are not accepted yet :( \n",
                      "Re-run converting to Ncategories-1 dummies")
}
