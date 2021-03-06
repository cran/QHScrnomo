% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tenf.crr.R
\name{tenf.crr}
\alias{tenf.crr}
\title{Ten fold cross validation for competing risks regression}
\usage{
tenf.crr(fit, time = NA, lps = FALSE, fold = 10)
}
\arguments{
\item{fit}{a competing risks regression model fittd by function
\code{\link[QHScrnomo]{crr.fit}}.}

\item{time}{the expected time point.}

\item{lps}{logical flag. If true, values of predicted X beta will be output
instead of cumulative incidence}

\item{fold}{number of fold. the default is 10 fold cross validation.}
}
\value{
A vector of predicted values of cumulative incidence or X beta for
  each observation.
}
\description{
Ten fold cross validation for crr endpoint
}
\details{
Do cross validation on a competing risk regression model.
}
\note{
Before the function is called, packages 'Hmisc', 'rms' and 'cmprsk'
should be loaded as the function will call some funcitons in these packages.
}
\examples{

\donttest{
data(prostate.dat)
dd <- datadist(prostate.dat)
options(datadist = "dd")
prostate.f <- cph(Surv(TIME_EVENT,EVENT_DOD == 1) ~ TX  + rcs(PSA,3) +
           BX_GLSN_CAT +  CLIN_STG + rcs(AGE,3) +
           RACE_AA, data = prostate.dat,
           x = TRUE, y= TRUE, surv=TRUE,time.inc = 144)
prostate.crr <- crr.fit(prostate.f,cencode = 0,failcode = 1)

## ten fold cross validation
prostate.dat$preds.tenf<-
    tenf.crr(prostate.crr,time = 120)
}

}
\seealso{
\code{\link[QHScrnomo]{crr.fit}}, \code{\link[cmprsk]{crr}}
}
\author{
Changhong Yu, Michael Kattan, Ph.D \cr Department of Quantitative
  Health Sciences\cr Cleveland Clinic\cr
}
\keyword{models}
\keyword{survival}
