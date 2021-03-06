% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Step3_PolicyGeneration.R
\name{genPortInception}
\alias{genPortInception}
\title{Generate Portfolio at Inception}
\usage{
genPortInception(
  birthDayRng = c("1950-01-01", "1980-01-01"),
  issueRng = c("2001-08-01", "2014-01-01"),
  matRng = c(15, 30),
  acctValueRng = c(50000, 5e+05),
  femPct = 0.4,
  fundFee = c(30, 50, 60, 80, 10, 38, 45, 55, 47, 46),
  baseFee = 200,
  prodPct = rep(1/19, 19),
  prodType = c("DBRP", "DBRU", "DBSU", "ABRP", "ABRU", "ABSU", "IBRP", "IBRU", "IBSU",
    "MBRP", "MBRU", "MBSU", "WBRP", "WBRU", "WBSU", "DBAB", "DBIB", "DBMB", "DBWB"),
  riderFee = c(25, 35, 35, 50, 60, 60, 60, 70, 70, 50, 60, 60, 65, 75, 75, 75, 85, 75,
    90),
  rollUpRate = rep(5, 19),
  withdrawalRate = rep(5, 19),
  numPolicy = 10
)
}
\arguments{
\item{birthDayRng}{A vector of two strings in 'YYYY-MM-DD' of birthday range.}

\item{issueRng}{A vector of two strings in 'YYYY-MM-DD' of issue date range.}

\item{matRng}{A vector of two integers, range of policy maturity.}

\item{acctValueRng}{A vector of two doubles, range of initial account values.}

\item{femPct}{A double, percentage of female policyholders in the
portfolio.}

\item{fundFee}{A vector of doubles, fees charged by each fund in bps.}

\item{baseFee}{A double, base fee for all funds in bps.}

\item{prodPct}{A vector of non-negative doubles, proportions of rider types.}

\item{prodType}{A vector of strings, names of different rider types.}

\item{riderFee}{A vector of doubles, rider fees for different riders in bps.}

\item{rollUpRate}{A vector of doubles, roll up rates for different rider
types in bps.}

\item{withdrawalRate}{A vector of doubles, withdrawal rates for different
rider types in bps.}

\item{numPolicy}{An integer, number of each type of policies to be generated.}
}
\value{
Outputs a data frame of 45 columns of attributes in an annuity
  contract.
}
\description{
Generate a portfolio of VA contracts at inception based on given attribute
ranges and investment fund information.
}
\examples{
genPortInception(c("1980-01-01", "1990-01-01"), c("2001-08-01", "2014-01-01"),
c(15, 30), c(5e4, 5e5), 0.4, c(30, 50, 60, 80, 10, 38, 45, 55, 47, 46),
200, rep(1 / 4, 4), c("WBRP", "WBRU", "WBSU", "DBWB"),
riderFee = c(25, 35, 35, 50), rep(5, 4), rep(5, 4), 100)
\dontrun{
genPortInception()
}
}
