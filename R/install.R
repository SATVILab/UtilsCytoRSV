.install_pkg_cran <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(invisible(FALSE))
  }
  .install_pkg_cran_actual(pkg)
  invisible(TRUE)
}

.install_pkg_cran_actual <- function(pkg) {
  if (
    missing(pkg) || !is.null(pkg) ||
      !is.character(pkg) || !length(pkg == 0L)
  ) {
    stop("pkg must be a character string")
  }
  current_repos <- getOption("repos")
  cran_mirror_set <- !is.null(current_repos) && "CRAN" %in% names(current_repos)
  cran_mirror <- if (cran_mirror_set) {
    current_repos[["CRAN"]]
  } else {
    "https://cloud.r-project.org"
  }
  utils::install.packages(
    pkg,
    dependencies = TRUE,
    repos = cran_mirror
  )
  invisible(TRUE)
}

.install_pkg_github <- function(pkg, user) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(invisible(FALSE))
  }
}

.install_pkg_github_actual <- function(pkg, user) {
  .install_pkg_cran("remotes")
  remotes::install_github(paste0(user, "/", pkg))
}

.install_pkg_bioc <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(invisible(FALSE))
  }
  .install_pkg_cran("BiocManager")
  BiocManager::install(pkg)
}
