% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_het_var.R
\name{extract_het_var}
\alias{extract_het_var}
\alias{extract_het_var.lme}
\alias{extract_het_var.glmmTMB}
\alias{extract_het_var.brmsfit}
\title{Extract heterogeneous variances}
\usage{
extract_het_var(model, digits = 3, scale = "var", ...)

\method{extract_het_var}{lme}(model, digits = 3, scale = "var", ...)

\method{extract_het_var}{glmmTMB}(model, digits = 3, scale = "var", ...)

\method{extract_het_var}{brmsfit}(
  model,
  digits = 3,
  scale = "var",
  ...,
  ci_level = 0.95,
  return_all = FALSE
)
}
\arguments{
\item{model}{An appropriate mixed model.}

\item{digits}{Rounding. Default is 3.}

\item{scale}{Return result on original standard deviation scale ('sd') or as
variance ('var'), the default.}

\item{...}{Other arguments specific to the method. Unused at present.}

\item{ci_level}{For brms objects, confidence level < 1, typically above
0.90. A value of 0 will not report it. Default is .95.}

\item{return_all}{For brms class objects, return all fitted values (\code{TRUE}) or only
the distinct ones. Default is \code{FALSE}.}
}
\value{
A vector of the estimates on the variance scale
}
\description{
Extract heterogeneous variances for nlme, glmmTMB, and brms
models.
}
\details{
For nlme models with heterogeneous variance, i.e. that contain
something like \code{varIdent(form = ~1|Group)}, this returns a more presentable
version the estimates. Only tested with the \code{varIdent} case.

For glmmTMB, this serves as a wrapper for \link{extract_cor_structure} for
models with for the \code{diag} function as part of the formula.  See that
function for details. For distributional models where the dispersion is
explicitly modeled separately via \verb{disp = ~ }, use the \code{component} argument
of the other functions in this package.

For brms distributional models with a \verb{sigma ~ . formula}, this produces the
(unique) fitted values for the dispersion part of the model.  As this is
often just a single grouping variable to allow variance to vary over the
group levels, only the distinct fitted values, which would be one value per
group, are returned. If all fitted values are desired, set \code{return_all} to
\code{TRUE}.

This function has not been tested except in the more simple model settings.
It's unclear how well it will work with other model complications added.
}
\examples{
library(nlme)
library(mixedup)

model <- lme(
  distance ~ age + Sex,
  data = Orthodont,
  random = ~ 1|Subject,
  weights = varIdent(form = ~ 1 | Sex)
)

summary(model)

extract_het_var(model)

library(glmmTMB)

# does not get the same estimates as nlme, but would get similar if modeled
# using dispersion approach.
model <-
  glmmTMB(distance ~ age + Sex + (1 | Subject) + diag(Sex + 0 | Subject),
          data = Orthodont)

extract_het_var(model)

# compare with
model <-
  glmmTMB(distance ~ age + Sex + (1 | Subject), dispformula = ~ Sex,
          data = Orthodont)

extract_fixed_effects(model, component = 'disp', exponentiate = TRUE)

\dontrun{
library(brms)

model <-
  brm(bf(distance ~ age + Sex + (1 | Subject), sigma ~ Sex),
      data = Orthodont)

extract_het_var(model)
}

}
\seealso{
Other extract: 
\code{\link{extract_cor_structure}()},
\code{\link{extract_fixed_effects}()},
\code{\link{extract_model_data}()},
\code{\link{extract_random_coefs}()},
\code{\link{extract_random_effects}()},
\code{\link{extract_vc}()}
}
\concept{extract}
