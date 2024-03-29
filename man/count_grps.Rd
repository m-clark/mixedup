% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_grps.R
\name{count_grps}
\alias{count_grps}
\alias{count_grps.default}
\alias{count_grps.merMod}
\alias{count_grps.glmmTMB}
\alias{count_grps.lme}
\alias{count_grps.brmsfit}
\alias{count_grps.stanreg}
\alias{count_grps.stanmvreg}
\alias{count_grps.gam}
\title{Count groups}
\usage{
count_grps(model, grp_vars)

\method{count_grps}{default}(model, grp_vars)

\method{count_grps}{merMod}(model, grp_vars)

\method{count_grps}{glmmTMB}(model, grp_vars)

\method{count_grps}{lme}(model, grp_vars)

\method{count_grps}{brmsfit}(model, grp_vars)

\method{count_grps}{stanreg}(model, grp_vars)

\method{count_grps}{stanmvreg}(model, grp_vars)

\method{count_grps}{gam}(model, grp_vars)
}
\arguments{
\item{model}{A fitted model e.g. from \code{lme4}.}

\item{grp_vars}{A character vector for the grouping/cluster variables used for
random effects}
}
\value{
A tibble of the results.
}
\description{
A helper function to count groups to add to random effect
results.
}
\details{
For each grouping variable for which random effects are estimated,
count the respective group sizes.  This is not meant to be used directly.
}
\examples{

library(lme4)
library(mixedup)

mod = lmer(Reaction ~ Days + (1 + Days | Subject), sleepstudy)

count_grps(mod, 'Subject')

}
\keyword{internal}
