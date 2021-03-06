% Generated by roxygen2 (4.0.2.9000): do not edit by hand
% Please edit documentation in R/gmailr.R
\name{gmail_auth}
\alias{gmail_auth}
\title{Setup oauth authentication for your gmail}
\usage{
gmail_auth(secret_file, scope = c("read_only", "modify", "compose", "full"))
}
\arguments{
\item{secret_file}{the secret json file downloaded from \url{https://cloud.google.com/console#/project}}

\item{scope}{the authentication scope to use}
}
\description{
Setup oauth authentication for your gmail
}
\examples{
\dontrun{
gmail_auth("file", "compose")
}
}

