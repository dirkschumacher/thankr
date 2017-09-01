#' Check if you starred packages on Github
#'
#' Starring a package on Github is a form of appreciation for the project.
#' With this function you can quickly check if you starred all your favorite packages.
#'
#' Still under development. Api and names can change.
#'
#' The function assumes that you have set an environment variable
#' (either \code{GITHUB_PAT} or \code{GITHUB_TOKEN}) with a Github Access Token.
#'
#' When using this function together with \code{\link{shoulders}} please note that
#' once run for the first time additional packages are attached to your session.
#'
#' Also take a look at the API documentation <https://developer.github.com/v3/> of Github
#' and their terms of use before using this function.
#'
#' @param packages a non empty character vector of package names.
#' @param console_output Optional. TRUE if the result should be written to the console
#' @param api_url Optional. The GitHub API url.
#' @param user_agent Optional. The user-agent for all the API requests.
#'                   Change it if this function is part of something bigger.
#'
#' @return
#' Always returns a data.frame with three columns "username", "repository" and "starred".
#' If \code{console_output} is TRUE, then the return value is invisible.
#'
#' @references Inspired by <https://github.com/musically-ut/appreciate>
#'
#' @export
gh_starred <- function(packages, console_output = TRUE,
                       api_url = "https://api.github.com",
                       user_agent = "https://github.com/dirkschumacher/thankr") {
  stopifnot(is.character(packages))
  packages <- packages[packages %in% utils::installed.packages()]
  packages <- unique(packages)
  if (length(packages) == 0) {
    stop("Non of the packages exist in your library", call. = FALSE)
  }
  urls <- unlist(lapply(packages, get_package_url))
  gh_repos <- extract_user_repo(urls)
  if (nrow(gh_repos) == 0) {
    gh_repos$starred <- logical(0)
  } else {
    gh_repos$starred <- is_starred(gh_repos, api_url = api_url, user_agent = user_agent)
  }
  gh_repos <- gh_repos[order(gh_repos$username, gh_repos$repository), ]
  if (console_output) {
    output_starred_repos(gh_repos)
    invisible(gh_repos)
  } else {
    gh_repos
  }
}

#' Appreciate your package dependencies
#'
#' This function is meant to be used when developing a package.
#' It extracts all package dependencies from the \code{DESCRIPTION} file in your working directory
#' and checks if you have starred it on github (if those packages are hosted on github).
#'
#' Still under development. Api and names can change.
#'
#' The function assumes that you have set an environment variable
#' (either \code{GITHUB_PAT} or \code{GITHUB_TOKEN}) with a Github Access Token.
#'
#' When using this function together with \code{\link{shoulders}} please note that
#' once run for the first time additional packages are attached to your session.
#'
#' Also take a look at the API documentation <https://developer.github.com/v3/> of Github
#' and their terms of use before using this function.
#'
#' @return
#' Always returns an invisible data.frame with three columns "username", "repository" and "starred".
#' Also outputs that information to the console.
#'
#' @references Inspired by <https://github.com/musically-ut/appreciate>
#'
#' @export
appreciate <- function() {
  is_package <- file.exists("DESCRIPTION")
  if (!is_package) {
    stop("No file DESCRIPTION found in your working directory.", call. = FALSE)
  }
  desc <- utils::packageDescription(".", lib.loc = ".",
                                    fields = c("Package", "Imports", "Suggests"))
  imports <- parse_dependencies(as.character(desc$Imports))
  suggests <- parse_dependencies(as.character(desc$Suggests))
  cat("Checking dependencies for", paste0(desc$Package, ":\n"))
  gh_starred(sort(unique(c(imports, suggests))))
}

# String -> String[]
# Parses a package dependency string and returns a character vector
# of package names
parse_dependencies <- function(dep_string) {
  dep_string <- dep_string[!is.na(dep_string)]
  if (length(dep_string) == 0L) {
    return(character(0L))
  }
  elements <- strsplit(dep_string, ",", fixed = TRUE)[[1]]
  elements <- trimws(elements)
  elements <- sub(pattern = "^([a-zA-Z0-9\\._]+).*",
                  replacement = "\\1",
                  x = elements, perl = TRUE)
  elements
}

output_starred_repos <- function(repos) {
  stopifnot(is.data.frame(repos))
  stopifnot(c("username", "repository", "starred") %in% colnames(repos))
  stars <- repos$starred
  gh_ids <- paste0(repos$username, "/", repos$repository)
  max_nchar <- max(nchar(gh_ids))
  for (i in seq_len(nrow(repos))) {
    starred <- stars[i]
    cat(
      sprintf(paste0("%-", max_nchar + 3, "s"), gh_ids[i]),
      if (starred) crayon::green("\u2605 Starred.") else crayon::yellow("\u2606 Not starred!"),
      "\n"
    )
  }
}

get_package_url <- function(pkg_name) {
  pkg_desc <- utils::packageDescription(pkg_name)
  url <- pkg_desc$URL
  if (length(url) == 0) {
    NA_character_
  } else {
    trimws(strsplit(url, ",|\\s+")[[1]])
  }
}

url_is_github <- function(url) {
  grepl(pattern = "^https{0,1}://github.com", url)
}

github_token <- function() {
  # same convention as in the gh package
  # try GITHUB_PAT, then GITHUB_TOKEN
  token <- Sys.getenv("GITHUB_PAT", "")
  no <- function(x) x == ""
  if (no(token)) {
    token <- Sys.getenv("GITHUB_TOKEN", "")
  }
  if (no(token)) {
    stop("Could not find a github token in either the variables ",
         "GITHUB_PAT or GITHUB_TOKEN",
         call. = FALSE)
  }
  token
}

# GithubUrl URL -> data.frame
extract_user_repo <- function(urls) {
  stopifnot(is.character(urls))
  github_urls <- urls[url_is_github(urls)]
  pattern <- "^https{0,1}://github.com/([a-zA-Z0-9\\-_.]+)/([a-zA-Z0-9\\-_.]+).*"
  username <- sub(pattern, "\\1", github_urls, perl = TRUE)
  repository <- sub(pattern, "\\2", github_urls, perl = TRUE)
  data.frame(username, repository, stringsAsFactors = FALSE)
}

# character -> logical
# side effects
is_starred <- function(repos, api_url, user_agent, verbose = FALSE) {
  stopifnot(is.data.frame(repos))
  stopifnot(c("username", "repository") %in% colnames(repos))
  stopifnot(is.character(repos$username), is.character(repos$repository))
  api_urls <- paste0(
    api_url,
    "/user/starred/",
    repos$username,
    "/",
    repos$repository
  )
  gh_repos <- repos
  gh_repos$api_url <- api_urls
  check_stars_gh(gh_repos, verbose = verbose)
}

# character -> logical
# side effects
check_stars_gh <- function(gh_repos,
                           user_agent = "https://github.com/dirkschumacher/thankr",
                           verbose = FALSE) {
  stopifnot(is.data.frame(gh_repos))
  headers <- list("Authorization" = paste0("token ", github_token()),
                  "User-Agent" = user_agent,
                  "Accept" = "application/vnd.github.v3+json")
  reqs <- lapply(gh_repos$api_url, function(x) {
    crul::HttpClient$new(
      url = x,
      headers = headers,
      opts = list(verbose = verbose)
    )$head()
  })
  # it seems there is a bug with async. Need to switch back to sync requests
  #out <- crul::AsyncVaried$new(.list = reqs)
  #out$request()
  #result <- out$status_code() == 204
  result <- vapply(reqs, function(x) x$status_code == 204, logical(1))

  # reorder the async results
  #pos <- vapply(out$responses(),
  #              function(x) which(gh_repos$api_url == x$request$url),
  #              numeric(1))
  #result[pos]
  result
}
