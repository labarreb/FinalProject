##' Select and Compute Regression Model (Description)
##'
##' The user inputs which model is chosen with the x parameter.
##' Then, the user provides the model formula and dataset.
##' A model is then created with those specifications (Details)
##' @title Regression Model Choice
##' @param x input for choice of model
##' @param frmla model formula as string
##' @param dtst dataset for model
##' @return a regression model with inputted specifications
##' @author c(Brian LaBarre, Silvia Jakubski)
##' @export
##' @examples
##' survmod(1,'mpg~wt',mtcars)


regr <- function(x, frmla, dtst) {
    if (x == "1") 
        return(lm(as.formula(frmla), data = dtst))
    if (x == "2") 
        return(glm(as.formula(frmla), family = poisson(), data = dtst))
    if (x == "3") 
        return(glm.nb(as.formula(frmla), data = dtst, control = glm.control(maxit = 1000)))
    if (x == "4") 
        return(glm(as.formula(frmla), family = binomial(), data = dtst))
    if (x == "5") 
        return(glm(as.formula(frmla), family = binomial(link = "probit"), 
            data = dtst))
}
