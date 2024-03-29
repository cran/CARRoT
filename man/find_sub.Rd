% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/carrot_functions_rsq.R
\name{find_sub}
\alias{find_sub}
\title{Finds certain subsets of predictors}
\usage{
find_sub(a,s,j,c,st)
}
\arguments{
\item{a}{A \code{j} x N matrix, containing all possible subsets (N overall) of the size \code{j} of predictors' indices.}

\item{s}{array of numbers of the size N}

\item{j}{number of rows in \code{a}}

\item{c}{array of all indices of the predictors}

\item{st}{a subset of predictors to be always included into a predictive model}
}
\value{
Returns a submatrix of matrix \code{a} which consits of columns determined by the input array \code{s}
}
\description{
Reorders the columns of matrix \code{a} according to the ordered elements of array \code{s}
}
\examples{
#all two-element subsets of 1:3

a<-combn(3,2)
s<-c(3,2,3)

find_sub(a,s,2,1:3)
}
