% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regr.R
\name{regr}
\alias{regr}
\title{Regression Model Choice}
\usage{
regr(x, frmla, dtst)
}
\arguments{
\item{x}{input for choice of model}

\item{frmla}{model formula as string}

\item{dtst}{dataset for model}
}
\value{
a regression model with inputted specifications
}
\description{
Select and Compute Regression Model (Description)
}
\details{
The user inputs which model is chosen with the x parameter.
Then, the user provides the model formula and dataset.
A model is then created with those specifications (Details)
}
\examples{
survmod(1,"mpg~wt",mtcars)
}
\author{
c(Brian LaBarre, Silvia Jakubski)
}
