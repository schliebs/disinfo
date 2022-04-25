% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_accounts.R
\name{get_accounts}
\alias{get_accounts}
\title{Get lists of Russian or Chinese Diplomats}
\usage{
get_accounts(country, group = c("diplomats"))
}
\arguments{
\item{country}{currently supports "RU" and "CN" (or both: c("RU","CN"))}

\item{group}{currently supports "diplomats"}
}
\value{
returns data.frame with twitter handles and details on dipomats
}
\description{
Get lists of Russian or Chinese Diplomats
}
\note{
tbc
}
\examples{
options(tidyverse.quiet = TRUE)
library(tidyverse)
ru_accs <- 
  get_accounts(country = "RU",group = "diplomats")

head(ru_accs,10)

ru_accs \%>\%
  group_by(cat) \%>\%
  mutate(n_cat = n()) \%>\%
  group_by(cat,n_cat,gov_label = gov_label != "") \%>\%
  summarise(n = n()) \%>\%
  mutate(perc = n/n_cat) \%>\% filter(gov_label == T)

}
\references{
tbc
}