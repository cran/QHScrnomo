% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anova.cmprsk.R
\name{anova.cmprsk}
\alias{anova.cmprsk}
\title{anova table for competing risks regression}
\usage{
\S3method{anova}{cmprsk}(object, \dots)
}
\arguments{
\item{object}{a competing risks regression model object
built from funciton \code{\link[QHScrnomo]{crr.fit}}}

\item{...}{other arguments}
}
\value{
anova table in matrix
}
\description{
generate anova table for crr
}
\details{
generate anova table for competing risks regression model
}
\examples{

data(prostate.dat)
dd <- datadist(prostate.dat)
options(datadist = "dd")
prostate.f <- cph(Surv(TIME_EVENT,EVENT_DOD == 1) ~ TX  + rcs(PSA,3) +
           BX_GLSN_CAT +  CLIN_STG + rcs(AGE,3) +
           RACE_AA, data = prostate.dat,
           x = TRUE, y= TRUE, surv=TRUE,time.inc = 144)
prostate.crr <- crr.fit(prostate.f,cencode = 0,failcode = 1)
## anova test
anova(prostate.crr)

}
