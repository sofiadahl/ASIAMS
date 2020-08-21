#' Intraclass Correlation.
#'
#' Calculate the ICC for a \code{brms} multilevel null (or unconditional) model
#' (see Details).
#'
#' @param x A \code{brms} null model fit.
#' @param indiv A character string; the name of the data-set column providing
#'   the identities of clusters for which varying intercepts have been modeled.
#'
#' @details The ICC is calculated from the standard deviation of individual (or
#'   varying) intercepts and the unexplained variation sigma as: \deqn{ ICC =
#'   sd(Intercept)^2 / (sd(Intercept)^2 + sigma^2) } The ICC reflects the amount
#'   of variation in the data set that can be explained by the grouping
#'   structure alone. Since this is calculated from a null model, the only
#'   grouping happens according to subjects/participants.
#' @references Gelman, A. & Hill, J. "Data Analysis Using Regression and
#'   Multilevel/Hierarchical Models." Cambridge University Press, 2007 (p. 258).
#' @return A real-valued scalar betwwen 0 and 1.
#' @author Michael Großbach (\email{michael.grossbach@hmtm-hannover.de})
#' @examples
#' \dontrun{
#'
#' bmod <- brms::brm(y ~ 1 + (1 | subject))
#' ICC(bmod, "subject")
#' }
ICC <- function(x = NULL, indiv = NULL) {
  stopifnot(brms::is.brmsfit(x),
            is.character(indiv))
  mod_summary <- summary(x)
  sd_Intercept <- mod_summary$random[[indiv]]["sd(Intercept)", "Estimate"]
  sigma <- mod_summary$spec_pars["sigma", "Estimate"]
  icc <- sd_Intercept^2 / (sd_Intercept^2 + sigma^2)
}
modify_prior <- function(x, prior = NULL, class = NULL, coef = NULL,
                         group = NULL, resp = NULL, dpar = NULL,
                         nlpar = NULL, bound = NULL) {

}
