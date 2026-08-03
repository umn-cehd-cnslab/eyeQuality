#' listBidsFiles - get list of all ET like files.
#'
#' @param directory path for data file as .tsv
#' @param subjectPattern_regex regex match pattern for subjects, or NULL if you are not using subject directories in 'directory' path
#' @param sessionPattern_regex regex match pattern for sessions, or NULL if you are not using session directories in 'subject_directory' paths
#' @param modalityPattern_regex regex match pattern for specific modality, or NULL which will default to searching '.tsv' files
#' @param recursiveSearch boolean to search file directory recursively
#' @param ... additional parameters that may get passed from wrapper functions
#'
#' @importFrom stringr str_glue
#'
#' @return list of raw ET data files to in BIDS-like directory
#' @export
#'
listBidsFiles <-
  function(directory,
           subjectPattern_regex = "sub-[A-Z0-9]+",
           sessionPattern_regex = "ses-[0-9]+",
           modalityPattern_regex = NULL,
           recursiveSearch = FALSE,
           ...) {
    # check to see if there are any nested folders?
    subject_dirs <-
      list.dirs(directory, full.names = TRUE, recursive = FALSE)

    # initialize list of files to append as we search directory
    files <- list()
    directories_for_files <- list()
    skipped_dirs <- character(0)

    # Next we find the list the directories to check.
    if (identical(subject_dirs, character(0))) {
      # If there are no subject directories,
      # we assume BIDS files are all in one directory
      # which is the directory specified in the function call
      directories_for_files <- c(directories_for_files, directory)
    } else {
      # IF subject_dirs has subdirectories, we loop through those.
      for (subject_dir in subject_dirs) {
        if (is.null(subjectPattern_regex)) {
          # if the subjectPattern_regex was specified as NULL
          # we assume that we will go through every subfolder in directory for files
          directories_for_files <-
            c(directories_for_files, subject_dir)
        } else if (grepl(subjectPattern_regex, subject_dir)) {
          # if subjectPattern_regex was specified
          # we only check the subfolders that match subjectPattern_regex

          if (is.null(sessionPattern_regex)) {
            # if sessionPattern_regex was specified as NULL
            # we assume one session, and will search subject_dir for files
            directories_for_files <-
              c(directories_for_files, subject_dir)
          } else {
            # pull subdirectories in subject_dir
            session_dirs <-
              list.dirs(subject_dir,
                full.names = TRUE,
                recursive = FALSE
              )
            # loop through session directories to match
            for (session_dir in session_dirs) {
              # if the session_dir matches sessionPattern_regex
              # we add that session_dir to check for files
              if (grepl(sessionPattern_regex, session_dir)) {
                directories_for_files <- c(directories_for_files, session_dir)
              } else {
                print(
                  stringr::str_glue(
                    "Subdirectory name doesn't match sessionPattern_regex. Skipping {session_dir}"
                  )
                )
                skipped_dirs <- c(skipped_dirs, session_dir)
                next
              }
            } # end for loop for session_dirs
          } # end else where sessionPattern_regex defined
        } else {
          # Otherwise, a subdirectory does not match subjectPattern_regex
          print(
            stringr::str_glue(
              "Directory name doesn't match subjectPattern_regex. Skipping {subject_dir}"
            )
          )
          skipped_dirs <- c(skipped_dirs, subject_dir)
          next
        }
      } # end for loop for subject_dirs
    }

    # now we have our list of directories to check for files
    for (filedir in directories_for_files) {
      if (is.null(modalityPattern_regex)) {
        tsv_files <- list.files(
          filedir,
          pattern = "\\.tsv$",
          recursive = recursiveSearch,
          full.names = TRUE
        )
        files <- c(files, tsv_files)
      } else {
        tsv_files <- list.files(
          filedir,
          pattern = modalityPattern_regex,
          recursive = recursiveSearch,
          full.names = TRUE
        )
        files <- c(files, tsv_files)
      }
    }

    result <- unlist(files)
    if (is.null(result)) {
      result <- character(0)
    }
    attr(result, "skipped") <- skipped_dirs

    if (length(files) == 0) {
      print("WARNING: No files found. If you expect to have data files in your directory, please check your directory structure.")
      print("Confirm subjectPattern_regex and session_Pattern_regex correctly find file(s) in your subject/session directories.")
      print("Otherwise specify a modalityPattern_regex with the naming convention for your eyetracking files.")
      print("If you have 'subject_dir/session_dir/additional_directory/data_files.tsv' structure, specify recursiveSearch = TRUE.")

      return(result)
    } else {
      return(result)
    }
  }
