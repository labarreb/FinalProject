##' Initialize Prompt-Based Modeling (Description)
##'
##' The user is able to request a type of model
##' and then input any model options and outputs that he/she
##' wishes.  After a dataset is entered, inputs include: variables,
##' interaction terms, type of distribution, and type of regression.
##' Outputs include: summary statistics, plots for diagnostics,
##' point estimates, and confidence intervals.  Basic coin flip
##' and dice roll functions are included for entertainment. (Details)
##' @title Prompt-Based Functions for Modeling
##' @return Prompt Menu is Launched for Choice
##' @author c(Brian LaBarre, Silvia Jakubski)
##' @export
##' @examples
##' finalproj()

finalproj <- function() {
    a <- menu(c("Regression Model", "Survival Model", "Flip Coin", 
        "Roll Die"), graphics = TRUE, title = "What would you like to perform?")
    if (a == 1) {
        return(mlr())
    } else if (a == 2) {
        return(smod())
    } else if (a == 3) {
        return(rcoin())
    } else if (a == 4) {
        return(rdie())
    }
}
