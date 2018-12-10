slr <- function() {
    b <- readline(prompt = "Name of your dataset: ")
    bb <- get(b)
    c <- menu(names(bb), title = "Response Variable (X): ")
    cc <- names(bb)[c]
    d <- menu(names(bb), title = "Predictor Variable (X): ")
    dd <- names(bb)[d]
    e <- lm(get(cc) ~ get(dd), data = bb)
    f <- menu(c("Y", "N"), title = "Would you like a Summary (Y/N): ")
    if (f == "1") 
        summ <- summary(e)
    j <- menu(c("Y", "N"), title = "Would you like a plot?")
    if (j == "1") {
        jplot <- function() {
            plot(get(cc) ~ get(dd), data = bb, col = "green", 
                pch = 19, xlab = dd, ylab = cc, main = "Linear Regression Plot")
            abline(e)
        }
    }
    return(list(if (exists("summ")) {
        summ
    }, if (exists("jplot")) {
        jplot()
    }))
}

slr <- function() {
    b <- readline(prompt = "Name of your dataset: ")
    bb <- get(b)
    c <- menu(names(bb), title = "Response Variable (X): ")
    cc <- names(bb)[c]
    d <- menu(names(bb), title = "Predictor Variable (X): ")
    dd <- names(bb)[d]
    cd <- paste(dd, collapse = "+")
    dc <- paste(as.name(cc), "~", as.name(cd), sep = "")
    e <- lm(as.formula(dc), data = bb)
    f <- menu(c("Y", "N"), title = "Would you like a Summary (Y/N): ")
    if (f == "1") 
        summ <- summary(e)
    j <- menu(c("Y", "N"), title = "Would you like a plot?")
    if (j == "1") {
        jplot <- function() {
            plot(as.formula(dc), data = bb, col = "green", pch = 19, 
                xlab = dd, ylab = cc, main = "Linear Regression Plot")
            abline(e)
        }
    }
    return(list(if (exists("summ")) {
        summ
    }, if (exists("jplot")) {
        jplot()
    }))
}


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
    dd <- paste(cc, "~", sep = "")
    ddd <- paste(dd, d, sep = "")
    e <- lm(as.formula(ddd), data = bb)
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

finalproj <- function() {
    a <- menu(c("Linear Regression"), graphics = FALSE, title = "What would you like to perform?")
    ifelse(a == 1, return(mlr()), print("More to be added soon"))
}
