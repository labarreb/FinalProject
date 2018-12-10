library(MASS)
library(survival)

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

smod <- function() {
  b <- readline(prompt = "Name of your dataset: ")
  bb <- get(b)
  c2 <- menu(names(bb), title = "Time Variable: ")
  cc2 <- names(bb)[c2]
  c <- menu(names(bb), title = "Event Indicator Variable: ")
  cc <- names(bb)[c]
  cc <- paste("Surv(",cc2,",",cc,")",sep="")
  cb <- as.integer(readline(prompt = "How many covariates?: "))
  if (cb=="0") {ccc <- 1}
    else{
    c3 <- ccc <- numeric(cb)
    for (i in 1:cb) {
      c3[i] <- menu(names(bb), title = "Which Predictor Variables to Include: ")
      ccc[i] <- names(bb)[as.integer(c3[i])]
    }
  }
  d <- paste(ccc, collapse = "+")
  if (cb>1) {
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
  e <- survmod(ee, ddd, bb)
  h <- menu(c("Y", "N"), title = "Would you like a summary?")
  if (h == "1") 
    summ <- summary(e)
  if (cb==1 && length(unique(rats[,as.integer(c3)]))) {
    j <- menu(c("Y", "N"), title = "Would you like a plot of the survival curve?")
    if (j == "1") {
      kplot <- function() {
        par(mfrow = c(1, 1))
        plot(survfit(as.formula(ddd), data = bb),col = c(2,3), 
            xlab = cc2, ylab = "Survival", main = "Survival Curve")
        legend("bottomleft",col = c(2,3),lty=1,title=names(bb)[c3],legend =
                c(unique(bb[,as.integer(c3)])[1],unique(bb[,as.integer(c3)])[2]))
      }
    }
  }
  ifelse (ee=="4",pe <- exp(coef(e)),pe <- exp(-coef(e)))
  ifelse (ee=="4",ci <- exp(confint(e)[1:(as.integer(cb)+1),]),ci <- exp(-confint(e)[1:(as.integer(cb)+1),]))
  res <- list(if (exists("summ")) summ, 
              if (exists("kplot")) kplot(),
              print("Point Estimate of Ratio(s) = "), 
              print(pe),
              print("95% Confidence Interval of Ratio(s) = "), 
              print(ci)
              )
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

survlist <- c("Weibull Model", "Exponential Model", 
             "Log-Logistic Model", "Cox PH Model")

finalproj <- function() {
    a <- menu(c("Regression Model", "Survival Model", "Flip Coin", "Roll Die"), graphics = TRUE, 
        title = "What would you like to perform?")
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
