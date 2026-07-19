#' Resolve the brms backend
#'
#' Determines which Stan backend to use for \code{\link[brms]{brm}}. When
#' \code{backend = "cmdstanr"} (the default), checks that the \pkg{cmdstanr}
#' package is installed and that a CmdStan installation is available. Falls back
#' to \code{"rstan"} with a message if either check fails.
#'
#' @param backend Character string: \code{"cmdstanr"} or \code{"rstan"}.
#' @return A single character string (\code{"cmdstanr"} or \code{"rstan"}).
#' @keywords internal
hlpr_resolve_backend <- function(backend = "cmdstanr") {
  backend <- match.arg(backend, c("cmdstanr", "rstan"))

  if (backend == "cmdstanr") {
    if (!requireNamespace("cmdstanr", quietly = TRUE)) {
      message(
        "cmdstanr is not installed. Falling back to rstan.\n",
        "Install cmdstanr for ~9x faster sampling: ",
        "install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))"
      )
      return("rstan")
    }
    cmdstan_ok <- tryCatch(
      { cmdstanr::cmdstan_path(); TRUE },
      error = function(e) FALSE
    )
    if (!cmdstan_ok) {
      message(
        "CmdStan is not installed. Falling back to rstan.\n",
        "Install CmdStan for ~9x faster sampling: cmdstanr::install_cmdstan()"
      )
      return("rstan")
    }
  }

  backend
}
