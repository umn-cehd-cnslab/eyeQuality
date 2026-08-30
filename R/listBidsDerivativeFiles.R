#' listBidsDerivativeFiles
#'
#' @description
#' `listBidsDerivativeFiles()` gets list of all ET like files in  derivatives.
#'
#' @param directory path for data file as .tsv
#' @param subjectPattern_regex regex match pattern for subjects
#' @param sessionPattern_regex regex match pattern for sessions
#' @param modalityPattern_regex regex match pattern for specific modality
#' @param recursiveSearch boolean to search file directory recursively
#' @param derivativesDirectory_name name for derivatives directory, default 'derivatives'
#' @param pipelineDirectory_name name for pipeline processing directory, default 'eyeQuality-v1'
#' @param derivativePattern_regex regex match pattern for specific derivative files
#' @param ... additional parameters that may get passed from wrapper functions
#'
#' @importFrom stringr str_glue
#' @importFrom fs path_dir
#'
#' @return list of ET derivatives data files to in BIDS-like directory
#' @export
#'
listBidsDerivativeFiles <- function(directory,
                                    subjectPattern_regex = "sub-[A-Z0-9]+",
                                    sessionPattern_regex = "ses-[0-9]+",
                                    modalityPattern_regex = NULL,
                                    recursiveSearch = FALSE,
                                    derivativesDirectory_name = "derivatives",
                                    pipelineDirectory_name = "eyeQuality-v1",
                                    derivativePattern_regex = NULL,
                                    ...) {
  # check to see if there are any nested folders?
  bids_dirs <-
    listBidsFiles(directory, subjectPattern_regex = subjectPattern_regex, sessionPattern_regex = sessionPattern_regex, modalityPattern_regex = modalityPattern_regex, recursiveSearch = recursiveSearch)
  # list.dirs(directory, full.names = TRUE, recursive = FALSE)

  # initialize list of files to append as we search directory
  files <- list()
  directories_for_files <- list()

  # loop through files pulled from listBidFiles, to find deriv directories processed from pipeline
  for (bids_files in bids_dirs) {
    tmp_path <- fs::path_dir(bids_files)
    tmp_deriv_path <-
      fs::path(
        tmp_path,
        derivativesDirectory_name,
        pipelineDirectory_name
      )

    if (dir.exists(tmp_deriv_path)) {
      directories_for_files <- c(directories_for_files, tmp_deriv_path)
    } else {
      print(
        stringr::str_glue(
          "Derivatives directory name doesn't exist. Skipping {tmp_path}"
        )
      )
    }
  }

  # get only the unique directories, since listBidFiles may have multiple data files in a single directory.
  directories_for_files <- unique(directories_for_files)

  if (length(directories_for_files) == 0) {
    print("WARNING: No derivative directories found. You may need to process your data using eyeQuality.")
    print("If you have preprocessed data files in your directory, please check your directory structure.")
    print("Confirm derivativesDirectory_name and pipelineDirectory_name are correct in your subject/session directories.")
    print("If you have 'subject_dir/session_dir/additional_directory/data_files.tsv' structure, specify recursiveSearch = TRUE.")

    return(unlist(directories_for_files))
  }

  # now we have our list of directories to check for files
  for (filedir in directories_for_files) {
    if (is.null(derivativePattern_regex)) {
      tsv_files <- list.files(
        # fs::path(filedir, derivativesDirectory_name, pipelineDirectory_name),
        filedir,
        pattern = "\\.tsv$",
        recursive = recursiveSearch,
        full.names = TRUE
      )
      files <- c(files, tsv_files)
    } else {
      tsv_files <- list.files(
        # fs::path(filedir, derivativesDirectory_name, pipelineDirectory_name),
        filedir,
        pattern = derivativePattern_regex,
        recursive = recursiveSearch,
        full.names = TRUE
      )
      files <- c(files, tsv_files)
    }
  }

  if (length(files) == 0) {
    print("WARNING: No derivative files found. If you expect to have derivative files in your directory, please check your directory structure.")
    print("Try to specify a derivativePattern_regex with the naming convention for your derivative files.")
    print("If you have 'subject_dir/session_dir/additional_directory/data_files.tsv' structure, specify recursiveSearch = TRUE.")

    return(unlist(files))
  } else {
    return(unlist(files))
  }
}
