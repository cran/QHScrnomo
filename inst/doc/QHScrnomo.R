## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(QHScrnomo)

## -----------------------------------------------------------------------------
str(prostate.dat)

## -----------------------------------------------------------------------------
# Register the data set
dd <- datadist(prostate.dat)
options(datadist = "dd")

# Fit the Cox-PH model for the event of interest
prostate.f <- cph(Surv(TIME_EVENT,EVENT_DOD == 1) ~ TX  + rcs(PSA,3) +
           BX_GLSN_CAT + CLIN_STG + rcs(AGE,3) +
           RACE_AA, data = prostate.dat,
           x = TRUE, y= TRUE, surv=TRUE, time.inc = 144)
prostate.f

## -----------------------------------------------------------------------------
# Refit to a competing risks regression
prostate.crr <- crr.fit(prostate.f, cencode = 0, failcode = 1)
prostate.crr

## -----------------------------------------------------------------------------
class(prostate.crr)

## -----------------------------------------------------------------------------
summary(prostate.crr)

## -----------------------------------------------------------------------------
anova(prostate.crr)

## -----------------------------------------------------------------------------
time_of_interest <- 120 # In months, so 10 years

## -----------------------------------------------------------------------------
set.seed(123)
prostate.dat$preds.tenf <- tenf.crr(prostate.crr, time = time_of_interest)
str(prostate.dat$preds.tenf)

## -----------------------------------------------------------------------------
cindex(
  prob = prostate.dat$preds.tenf,
  fstatus = prostate.dat$EVENT_DOD,
  ftime = prostate.dat$TIME_EVENT,
  type = "crr",
  failcode = 1
)

## ----fig.width=5, fig.height=5------------------------------------------------
groupci(
  x = prostate.dat$preds.tenf,
  ftime = prostate.dat$TIME_EVENT,
  fstatus = prostate.dat$EVENT_DOD,
  g = 10, # Deciles
  u = time_of_interest,
  failcode = 1,
  xlab = "Predicted 10-year prostate cancer-specific mortality",
  ylab = "Actual 10-year prostate cancer-specific mortality"
)

## ----fig.width=7, fig.height=6------------------------------------------------
# Set some nice display labels (also see ?Newlevels)
prostate.g <-
  Newlabels(
    fit = prostate.crr,
    labels = 
      c(
        TX = "Treatment options",
        PSA = "PSA (ng/mL)",
        BX_GLSN_CAT = "Biopsy Gleason Score Sum",
        CLIN_STG = "Clinical Stage",
        AGE = "Age (Years)",
        RACE_AA = "Race"
      )
  )

# Construct the nomogram
nomogram.crr(
  fit = prostate.g,
  failtime = time_of_interest,
  lp = FALSE,
  xfrac = 0.65,
  fun.at = seq(0.2, 0.45, 0.05),
  funlabel = "Predicted 10-year risk"
)

## -----------------------------------------------------------------------------
sas.cmprsk(prostate.crr, time = time_of_interest)

## -----------------------------------------------------------------------------
# Get the cuminc object
cum <- 
  cmprsk::cuminc(
    ftime = prostate.dat$TIME_EVENT, 
    fstatus = prostate.dat$EVENT_DOD, 
    group = prostate.dat$TX,
    cencode = 0
  )

# Extract "nice" output at a time point of interest
pred.ci(cum, tm1 = time_of_interest, failcode = 1)

## -----------------------------------------------------------------------------
prostate.dat$pred.120 <- predict(prostate.crr, time = time_of_interest)
str(prostate.dat$pred.120)

