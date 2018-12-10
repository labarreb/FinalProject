##' Select and Compute Survival Model (Description)
##'
##' The user inputs which model is chosen with the x parameter.
##' Then, the user provides the model formula and dataset.
##' A model is then created with those specifications (Details)
##' @title Survival Model Choice
##' @param x input for choice of model
##' @param frmla model formula as string
##' @param dtst dataset for model
##' @return a survival model with inputted specifications
##' @author c(Brian LaBarre, Silvia Jakubski)
##' @export
##' @examples
##' survmod(1,'Surv(time,status)~1',rats)


survmod <- function(x, frmla, dtst) {
    if (x == "1") 
        return(survreg(as.formula(frmla), data = dtst))
    if (x == "2") 
        return(survreg(as.formula(frmla), data = dtst, dist = "exponential"))
    if (x == "3") 
        return(survreg(as.formula(frmla), data = dtst, dist = "loglogistic"))
    if (x == "4") 
        return(coxph(as.formula(frmla), data = dtst))
}
