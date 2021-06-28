#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

cat_function <- function(love=TRUE){
  if(love==TRUE){
    print("I love cats!")
  }
  else {
    print("I am not a cool person.")
  }
}

#' Another Cat Function
#'
#' This function allows you to express your love of cats even more.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function2()

cat_function2 <- function(love=TRUE){
  if(love==TRUE){
    print("I love cats!!")
  }
  else {
    print("I am not a cool person at all.")
  }
}

#' A test function
#'
#' test_function1()

test_function1 <- function(x){
  return(x^2)
}
