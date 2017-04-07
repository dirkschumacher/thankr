#' Find out who wrote the packages you use
#'
#' This function helps to identify what packages you depend on. It identifies the maintainers and the
#' number of packages they wrote.
#'
#' If you do a package lookup you connect to a CRAN mirror.
#'
#' @param where either look in the current loaded "namespace", your whole "library" or for a specific package
#' @param package if \code{where} is "package" then this must be a character vector of positive length
#' @param include_dependencies if \code{where} is "package" then use this to include all dependencies of the package.
#'                             Otherwise it will just return the maintainer.
#'
#' @return a tibble with three columns, \code{maintainer}, \code{no_packages}, \code{packages} showing all packages you depend on.
#' @export
shoulders <- function(where = c("namespace", "library", "package"),
                      package = NULL, include_dependencies = TRUE) {
  # !is.null(package) => is.logical(include_dependencies)
  stopifnot(is.null(package) || is.logical(include_dependencies))
  where <- match.arg(where)

  # where == "package" => is.character(package) && length(package) > 0
  stopifnot(where != "package" || is.character(package) && length(package) > 0)
  if (where == "namespace") {
    namespace_shoulders()
  } else if (where == "library") {
    library_shoulders()
  } else if (where == "package") {
    package_shoulders(package, include_dependencies = include_dependencies)
  } else {
    stop("should not happen", call. = FALSE)
  }
}

#' @noRd
get_maintainer <- function(pkg_name) {
  pkg_desc <- utils::packageDescription(pkg_name)
  maintainer <- pkg_desc$Maintainer
  if (length(maintainer) == 0) {
    NA_character_
  } else {
    maintainer
  }
}

#' @noRd
package_shoulders <- function(packages, include_dependencies = FALSE) {
  installed_packages <- utils::installed.packages()
  stopifnot(all(packages %in% installed_packages))
  stopifnot(length(include_dependencies) == 1)
  stopifnot(is.logical(include_dependencies))
  package_list <- unique(packages)
  if (include_dependencies) {
    dependencies <- tools::package_dependencies(packages, recursive = TRUE)
    package_list <- unique(c(package_list, unlist(dependencies)))
  }
  build_package_list(package_list)
}

#' @noRd
library_shoulders <- function() {
  build_package_list(utils::installed.packages()[, 1])
}

#' @noRd
namespace_shoulders <- function() {
  build_package_list(loadedNamespaces())
}

#' @noRd
build_package_list <- function(packages) {
  stopifnot(is.character(packages))
  maintainer <- vapply(packages, get_maintainer, character(1))
  df <- data.frame(pkg_name = packages, maintainer, stringsAsFactors = FALSE)
  format_pkg_df(df)
}

#' @noRd
format_pkg_df <- function(df) {
  stopifnot(is.data.frame(df))
  stopifnot(c("maintainer", "pkg_name") %in% colnames(df))
  df <- dplyr::filter_(df, ~!is.na(maintainer))
  df <- dplyr::group_by_(df, "maintainer")
  df <- dplyr::summarise_(df, no_packages = ~n(),
                          packages = ~paste0(sort(pkg_name), collapse = ", "))
  dplyr::arrange_(df, ~desc(no_packages))
}

