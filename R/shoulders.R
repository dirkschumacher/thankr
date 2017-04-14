#' Find Out Who Maintains the Packages you Use
#'
#' This function helps to identify what packages you depend on. It finds the maintainers and the
#' number of packages they maintain.
#'
#' Note, if you do a package lookup you connect to a CRAN mirror.
#'
#' @param where either look in the current loaded "session", your whole "library" or for a specific "package"
#' @param package if \code{where} is "package" then this must be a character vector of positive length
#' @param include_dependencies if \code{where} is "package" then use this to include all dependencies of the package.
#'                             Otherwise it will just return the maintainer.
#'
#' @return A data.frame showing all packages you depend on with these three columns:
#'         \itemize{
#'         \item{\code{maintainer}}{ The maintainer as in the DESCRIPTION file}
#'         \item{\code{no_packages}}{ The number of packages of that maintainer}
#'         \item{\code{packages}}{ A comma separated list of those packages}
#'         }
#' @export
shoulders <- function(where = c("session", "library", "package"),
                      package, include_dependencies = TRUE) {

  # input checks
  where <- match.arg(where)
  package <- if (missing(package)) NULL else package
  if (where == "package" &&
      (is.null(package) || !is.character(package) || length(package) == 0)) {
    stop("When doing a package lookup, please also specify at least one package",
         call. = FALSE)
  }
  # !missing(package) => is.logical(include_dependencies)
  stopifnot(missing(package) || is.logical(include_dependencies))

  if (where == "session") {
    session_shoulders()
  } else if (where == "library") {
    library_shoulders()
  } else if (where == "package") {
    package_shoulders(package, include_dependencies = include_dependencies)
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
  installed_packages <- packages[packages %in% utils::installed.packages()]
  not_installed_pkgs <- packages[!packages %in% installed_packages]
  if (length(installed_packages) == 0) {
    stop("None of the packages you provided exist in your local library",
         call. = FALSE)
  }
  if (length(not_installed_pkgs) > 0) {
    warning("The following packages do not exist in your local library and will be ignored: ",
            paste0(sort(not_installed_pkgs), collapse = ", "), call. = FALSE)
  }
  packages <- installed_packages
  stopifnot(length(include_dependencies) == 1)
  stopifnot(is.logical(include_dependencies))
  package_list <- unique(packages)
  if (include_dependencies) {
    dependencies <- tools::package_dependencies(packages,
                                                db = utils::available.packages(),
                                                recursive = TRUE)
    package_list <- unique(c(package_list, unlist(dependencies)))
  }
  build_package_list(package_list)
}

#' @noRd
library_shoulders <- function() {
  build_package_list(utils::installed.packages()[, 1])
}

#' @noRd
session_shoulders <- function() {
  pkgs <- loadedNamespaces()
  pkgs <- pkgs[pkgs != "thankr"]
  build_package_list(pkgs)
}

#' @noRd
build_package_list <- function(packages) {
  stopifnot(is.character(packages))
  format_pkg_df(lapply(packages, function(x) {
    list(maintainer = get_maintainer(x), pkg_name = x)
  }))
}

#' @noRd
format_pkg_df <- function(lpkgs) {
  stopifnot(is.list(lpkgs))

  # group the packages for each maintainer
  grouped_data <- Reduce(function(acc, el) {
    acc[[el$maintainer]] <- c(acc[[el$maintainer]], el$pkg_name)
    acc
  }, lpkgs, list())

  # convert it to a list of data.frames
  ldf <- Map(function(pkg_list) {
    data.frame(no_packages = length(pkg_list),
               packages = paste0(sort(pkg_list), collapse = ", "),
               stringsAsFactors = FALSE)
  }, grouped_data)

  # convert the list of data.frames to a single data.frame,
  # add the maintainer column and order it
  df <- cbind(maintainer = names(ldf),
               do.call(rbind, ldf),
               stringsAsFactors = FALSE)
  df <- df[!is.na(df$maintainer), ]
  df <- df[order(df$no_packages, decreasing = TRUE), ]
  rownames(df) <- NULL
  df
}

