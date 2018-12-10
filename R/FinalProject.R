library(MASS)

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

reglist <- c("Linear Regression", "Poisson (Count Outcome)", 
    "Negative Binomial (Count Outcome)", "Logistic Model (Binary Outcome)", 
    "Probit Regression (Binary Outcome)")

rdie <- function() {
    return(sample.int(6, 1))
}
rcoin <- function() {
    return(sample(c("Heads", "Tails"), 1))
}

finalproj <- function() {
    a <- menu(c("Regression", "Roll Die", "Flip Coin"), graphics = TRUE, 
        title = "What would you like to perform?")
    if (a == 1) {
        return(mlr())
    } else if (a == 2) {
        return(rdie())
    } else if (a == 3) {
        return(rcoin())
    }
}
