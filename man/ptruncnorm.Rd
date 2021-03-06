% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trunc-2.R
\name{ptruncnorm}
\alias{ptruncnorm}
\title{Truncated normal distribution}
\usage{
ptruncnorm(x, mu = 0, sigma = 1, a = -Inf, b = Inf)
}
\arguments{
\item{mu}{mean of `un-truncated` distribution}

\item{a}{minimum value}

\item{b}{maximum value}

\item{mu}{standard deviation of `un-truncated` distribution}
}
\value{
cumulative probability
}
\description{
Distribution function of truncated normali distribution. See \link[=https://en.wikipedia.org/wiki/Truncated_normal_distribution]{Wikipedia}
}
