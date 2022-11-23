
#' Scan files using using regular expressions.
#'
#' @param files character
#' @param patterns character
#' @param file_type character
#' @param encoding character
#' @param ignore.case boolean
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{scan_files("file-path", "regular-expression")}
scan_files <- function(
    files,
    patterns,
    file_type = NULL,
    encoding = "unknown",
    ignore.case = TRUE
) {

  # Check files input
  files <- .check_files(files)

  # Only keep files with the specified file type
  if (!is.null(file_type)) {
    tmp <- files
    files <- character(0)
    for (i in seq_along(tmp)) {
      if (grepl(paste0(file_type, "$"), tmp[i], ignore.case = TRUE)) {
        files <- c(files, tmp[i])
      }
    }
  }

  # Check regular expressions
  regexpr_list <- .check_patterns(patterns)

  # Create empty data.frame to store matches
  matches <- data.frame(
    file = character(),
    pattern = character(),
    line = numeric(),
    text = character()
  )

  # Search each file for each pattern and return information on matches.
  for (i in seq_along(files)) {
    any_match <- FALSE
    for (j in seq_along(patterns)) {
      tmp <- .find_matches(
        file = files[i],
        pattern = patterns[j],
        encoding = encoding,
        ignore.case = ignore.case
      )
      if (dim(tmp)[1] > 0L) any_match <- TRUE
      matches <- rbind(matches, tmp)
    }
    # If no matches are found for a file, add a row with NA pattern,
    # line, and text value
    if (isFALSE(any_match)) {
      tmp <- data.frame(
        file = files[i],
        pattern = character(1),
        line = NA_integer_,
        text = character(1)
      )
      matches <- rbind(matches, tmp)
    }
  }


  matches
}



#### Helper functions ####

#' List files in directory
#'
#' @param files string
#'
#' @return character
#' @keywords internal
#'
#' @examples
#' \dontrun{.files_in_directory(dir)}
.files_in_directory <- function(dir, recursive = TRUE) {
  # Check dir is a character string
  if(!(is.character(dir) & length(dir) == 1L)) {
    stop("'dir' is not a character string")
  }

  # Check dir is a directory
  if(!utils::file_test("-d", dir)) {
    stop("'dir' is not a directory or does not exist")
  }

  # Return files
  list.files(
    path = dir,
    full.names = TRUE,
    recursive = recursive,
    include.dirs = TRUE
  )
}

#' Check files argument input
#'
#' @param files character
#'
#' @return character
#' @keywords internal
#'
#' @examples
#' \dontrun{.check_files(files)}
.check_files <- function(files) {
  # Check files is a character vector
  if (!is.character(files)) {
    stop("'files' is not a character vector")
  }
  # Check each element in files is a file or a directory.
  for (i in seq_along(files)) {
    if (!utils::file_test("-f", files[i]) & !utils::file_test("-d", files[i])) {
      stop(paste0("'", files[i], "' does not exist"))
    }
  }

  # Replace directories
  new_files <- character(0)
  for (i in seq_along(files)) {
    if (utils::file_test("-d", files[i])) {
      new_files <- c(new_files, .files_in_directory(dir = files[i]))
    } else if (utils::file_test("-f", files[i])) {
      new_files <- c(new_files, files[i])
    }
  }

  new_files
}

#' Check patterns argument input
#'
#' @param patterns character
#'
#' @return character
#' @keywords internal
#'
#' @examples
#' \dontrun{.check_patterns(patterns)}
.check_patterns <- function(patterns) {
  # Check patterns is a character vector
  if (!is.character(patterns)) {
    stop("'patterns' is not a character vector")
  }

  patterns
}

#' Search file for regular expression matches
#'
#' @param file character
#' @param pattern characater
#' @param encoding characater
#'
#' @return data.frame
#' @keywords internal
#'
#' @examples
#' \dontrun{.find_matches(file, pattern)}
.find_matches <- function(
    file,
    pattern,
    encoding = "unknown",
    ignore.case = TRUE
) {

  # Create empty data.frame to store matches
  matches <- data.frame(
    file = character(),
    pattern = character(),
    line = numeric(),
    text = character()
  )

  file_content <- readLines(file, warn = FALSE, encoding = encoding)
  if (any(grepl(pattern, file_content, ignore.case = ignore.case))) {
    tmp <- data.frame(
      file = file,
      pattern = pattern,
      line = which(grepl(pattern, file_content, ignore.case = ignore.case)),
      text = grep(pattern, file_content, ignore.case = ignore.case, value = TRUE)
    )
    matches <- rbind(matches, tmp)
  }

  matches
}
