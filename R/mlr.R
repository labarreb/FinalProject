##' Initialize Prompt-Based Regression Modeling (Description)
##'
##' The user is able to request a type of model
##' and then input any model options and outputs that he/she
##' wishes.  After a dataset is entered, inputs include: variables,
##' interaction terms, type of distribution, and type of regression.
##' Outputs include: summary statistics, plots for diagnostics,
##' point estimates, and confidence intervals.  Basic coin flip
##' and dice roll functions are included for entertainment. (Details)
##' @title Prompt for Regression Modeling
##' @author c(Brian LaBarre, Silvia Jakubski)
##' @export
##' @examples
##' mlr()

mlr <- function() {
    b <- readline(prompt = "Name of your dataset: ")
    bb <- get(b)
    c <- menu(names(bb), title = "Response Variable (X): ")
    cc <- names(bb)[c]
    cb <- as.integer(readline(prompt = "How many predictor variables?: "))
    ccc <- numeric(cb)
    for (i in 1:cb) {
        ccc[i] <- menu(names(bb), title = "Which Predictor Variables to Include: ")
        ccc[i] <- names(bb)[as.integer(ccc[i])]
    }
    d <- paste(ccc, collapse = "+")
    intt <- menu(c("Y", "N"), title = "Would you like an interaction term?")
    if (intt == "1") {
        abc <- numeric(2)
        for (i in 1:2) {
            abc[i] <- menu(names(bb), title = "Which Predictor Variables to Include: ")
            abc[i] <- names(bb)[as.integer(abc[i])]
        }
        abc <- paste(abc, collapse = "*")
        d <- paste(d, abc, sep = "+")
    }
    dd <- paste(cc, "~", sep = "")
    ddd <- paste(dd, d, sep = "")
    ee <- menu(reglist, title = "Which Regression to Perform: ")
    e <- regr(ee, ddd, bb)
    h <- menu(c("Y", "N"), title = "Would you like a summary?")
    if (h == "1") 
        summ <- summary(e)
    j <- menu(c("Y", "N"), title = "Would you like a plot(s)?")
    if (j == "1" && cb != 1) {
        jplot <- function() {
            par(mfrow = c(2, 2))
            plot(e)
        }
    } else if (j == "1" && cb == 1) {
        kplot <- function() {
            par(mfrow = c(1, 1))
            plot(as.formula(ddd), data = bb, col = "green", pch = 19, 
                xlab = ccc[1], ylab = cc, main = "Linear Regression Plot")
            abline(e)
        }
    }
    res <- list(if (exists("summ")) {
        summ
    }, if (exists("jplot")) {
        jplot()
    }, if (exists("kplot")) {
        kplot()
    })
    return(res[!sapply(res, is.null)])
}
