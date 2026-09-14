# Create a custom function that creates a data_design object
data_design <- function(stimulus, response, rt, correct, id = NULL) {
  
  design <- list(
    stimulus = rlang::enquo(stimulus),
    response = rlang::enquo(response),
    rt = rlang::enquo(rt),
    correct = rlang::enquo(correct),
    id = rlang::enquo(id)
  )
  
  class(design) <- "data_design"
  
  return(design)
}
