##' Initialize Prompt-Based Survival Data Modeling (Description)
##'
##' The user is able to request a type of model
##' and then input any model options and outputs that he/she
##' wishes.  After a dataset is entered, inputs include: variables,
##' interaction terms, type of distribution, and type of regression.
##' Outputs include: summary statistics, plots for diagnostics,
##' point estimates, and confidence intervals.  Basic coin flip
##' and dice roll functions are included for entertainment. (Details)
##' @title Prompt for Survival Data Modeling
##' @author c(Brian LaBarre, Silvia Jakubski)
##' @export
##' @examples
##' smod()

smod <- function() {
    b <- readline(prompt = "Name of your dataset: ")
    bb <- get(b)
    c2 <- menu(names(bb), title = "Time Variable: ")
    cc2 <- names(bb)[c2]
    c <- menu(names(bb), title = "Event Indicator Variable: ")
    cc <- names(bb)[c]
    cc <- paste("Surv(", cc2, ",", cc, ")", sep = "")
    cb <- as.integer(readline(prompt = "How many covariates?: "))
    if (cb == "0") {
        ccc <- 1
    } else {
        c3 <- ccc <- numeric(cb)
        for (i in 1:cb) {
            c3[i] <- menu(names(bb), title = "Which Predictor Variables to Include: ")
            ccc[i] <- names(bb)[as.integer(c3[i])]
        }
    }
    d <- paste(ccc, collapse = "+")
    if (cb > 1) {
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
    }
    dd <- paste(cc, "~", sep = "")
    ddd <- paste(dd, d, sep = "")
    ee <- menu(survlist, title = "Which Survival Model?: ")
    return(ee)
    e <- survmod(ee, ddd, bb)
    h <- menu(c("Y", "N"), title = "Would you like a summary?")
    if (h == "1")
        summ <- summary(e)
    if (cb == 1 && ee %in% c(1:3) && length(unique(rats[, as.integer(c3)]))) {
        j <- menu(c("Y", "N"), title = "Would you like a plot of the survival curve?")
        if (j == "1") {
            kplot <- function() {
                par(mfrow = c(1, 1))
                plot(survfit(as.formula(ddd), data = bb), col = c(2,
                  3), xlab = cc2, ylab = "Survival", main = "Survival Curve")
                legend("bottomleft", col = c(2, 3), lty = 1,
                  title = names(bb)[c3], legend = c(unique(bb[,
                    as.integer(c3)])[1], unique(bb[, as.integer(c3)])[2]))
            }
        }
    }
    ifelse(ee == "4", pe <- exp(coef(e)), pe <- exp(-coef(e)))
    ifelse(ee == "4", ci <- exp(confint(e)[1:(as.integer(cb)), ]), ci <- exp(-confint(e)[1:(as.integer(cb) + 1),
        ]))
    res <- list(if (exists("summ")) summ, if (exists("kplot")) kplot(),
        print("Point Estimate of Ratio(s) = "), print(pe), print("95% Confidence Interval of Ratio(s) = "),
        print(ci))
    return(res[!sapply(res, is.null)])
}
