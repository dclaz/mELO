% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/est_A_from_df.R
\name{est_A_from_data}
\alias{est_A_from_data}
\title{Get very rough estimates of the advantage/disadvantage matrix from match data}
\usage{
est_A_from_data(data)
}
\arguments{
\item{data}{a data.frame of matches. Four columns, time, agent 1, agent 2 and the outcome.}
}
\value{
a matrix.
}
\description{
Combined with est_C_given_A(), this might be useful for warm starting mELO models.
}
\examples{
est_A_from_data(rps_df)
est_A_from_data(rpssl_df)
est_A_from_data(rpsfw_df)
noisy_rpsfw_df <- rpsfw_df
noisy_rpsfw_df$outcome <- add_noise_to_outcomes(noisy_rpsfw_df$outcome, error_prob = 0.5)
est_A_from_data(noisy_rpsfw_df)
}
