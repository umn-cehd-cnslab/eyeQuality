#' Eye selection
#'
#' @param data dataframe
#' @param eyeSelection_method "Maximize", "Strict", "Left", or "Right"
#' @param gazeLeftX string column name for data containing left eye X-axis gazepoints
#' @param gazeLeftY string column name for data containing left eye Y-axis gazepoints
#' @param gazeRightX string column name for data containing right eye X-axis gazepoints
#' @param gazeRightY string column name for data containing right eye Y-axis gazepoints
#' @param pupilLeft string column name for data containing left eye pupil data
#' @param pupilRight string column name for data containing right eye pupil data
#' @param distanceLeftZ string column name for data containing left eye distance from screen in mm (Z distance)
#' @param distanceRightZ string column name for data containing right eye distance from screen in mm  (Z distance)
#' @param ... additional passed parameters from parent function
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rowwise
#' @importFrom rlang sym
#'
#' @return data
#' @export
#'
eyeSelection <-
  function(data,
           eyeSelection_method = "Maximize",
           gazeLeftX = "gazeLeftX.int",
           gazeLeftY = "gazeLeftY.int",
           gazeRightX = "gazeRightX.int",
           gazeRightY = "gazeRightY.int",
           pupilLeft = "pupilLeft.int",
           pupilRight = "pupilRight.int",
           distanceLeftZ = "distanceLeftZ.int",
           distanceRightZ = "distanceRightZ.int",
           ...) {
    # hacky way to get rid of "no visible binding for global variable" note
    distanceZ.eyeSelect <- gazeX.eyeSelect <- gazeY.eyeSelect <- pupil.eyeSelect <- NULL

    # Maximize option - replace NAs if data exists for one eye and not the other
    if (eyeSelection_method == "Maximize") {
      print("selecting eye based on maximized approach")
      # When one eye/pupil is == NA replace with the other eye/pupil
      # Gaze
      data[["gpLeft.X.temp"]] <-
        ifelse(is.na(data[[gazeLeftX]]), data[[gazeRightX]], data[[gazeLeftX]])
      data[["gpLeft.Y.temp"]] <-
        ifelse(is.na(data[[gazeLeftY]]), data[[gazeRightY]], data[[gazeLeftY]])
      data[["gpRight.X.temp"]] <-
        ifelse(is.na(data[[gazeRightX]]), data[[gazeLeftX]], data[[gazeRightX]])
      data[["gpRight.Y.temp"]] <-
        ifelse(is.na(data[[gazeRightY]]), data[[gazeLeftY]], data[[gazeRightY]])
      # Pupils
      data[["pupilLeft.temp"]] <-
        ifelse(is.na(data[[pupilLeft]]), data[[pupilRight]], data[[pupilLeft]])
      data[["pupilRight.temp"]] <-
        ifelse(is.na(data[[pupilRight]]), data[[pupilLeft]], data[[pupilRight]])
      # Dist
      data[["distLeft.Z.temp"]] <-
        ifelse(is.na(data[[distanceLeftZ]]), data[[distanceRightZ]], data[[distanceLeftZ]])
      data[["distRight.Z.temp"]] <-
        ifelse(is.na(data[[distanceRightZ]]), data[[distanceLeftZ]], data[[distanceRightZ]])

      # Set eye selection parameter
      # Gaze
      data[["gazeX.es.selection"]] <- ifelse(
        !is.na(data[[gazeLeftX]]) &
          !is.na(data[[gazeRightX]]),
        "mean_maximized",
        ifelse(
          !is.na(data[[gazeLeftX]]) & is.na(data[[gazeRightX]]),
          "left_only",
          ifelse(
            is.na(data[[gazeLeftX]]) & !is.na(data[[gazeRightX]]),
            "right_only",
            ifelse(is.na(data[[gazeLeftX]]) &
              is.na(data[[gazeRightX]]), "both_na",
            "other_maximized"
            )
          )
        )
      )
      data[["gazeY.es.selection"]] <- ifelse(
        !is.na(data[[gazeLeftY]]) &
          !is.na(data[[gazeRightY]]),
        "mean_maximized",
        ifelse(
          !is.na(data[[gazeLeftY]]) & is.na(data[[gazeRightY]]),
          "left_only",
          ifelse(
            is.na(data[[gazeLeftY]]) & !is.na(data[[gazeRightY]]),
            "right_only",
            ifelse(is.na(data[[gazeLeftY]]) &
              is.na(data[[gazeRightY]]), "both_na",
            "other_maximized"
            )
          )
        )
      )
      # Pupils
      data[["pupil.es.selection"]] <- ifelse(
        !is.na(data[[pupilLeft]]) &
          !is.na(data[[pupilRight]]),
        "mean_maximized",
        ifelse(
          !is.na(data[[pupilLeft]]) & is.na(data[[pupilRight]]),
          "left_only",
          ifelse(
            is.na(data[[pupilLeft]]) & !is.na(data[[pupilRight]]),
            "right_only",
            ifelse(is.na(data[[pupilLeft]]) &
              is.na(data[[pupilRight]]), "both_na",
            "other_maximized"
            )
          )
        )
      )
      # Dist
      data[["distZ.es.selection"]] <- ifelse(
        !is.na(data[[distanceLeftZ]]) &
          !is.na(data[[distanceRightZ]]),
        "mean_maximized",
        ifelse(
          !is.na(data[[distanceLeftZ]]) & is.na(data[[distanceRightZ]]),
          "left_only",
          ifelse(
            is.na(data[[distanceLeftZ]]) & !is.na(data[[distanceRightZ]]),
            "right_only",
            ifelse(is.na(data[[distanceLeftZ]]) &
              is.na(data[[distanceRightZ]]), "both_na",
            "other_maximized"
            )
          )
        )
      )

      # print(names(data))

      # Calculate maximized averages
      print(
        "calculating maximized averages for gazeX.eyeSelect, gazeY.eyeSelect, pupil.eyeSelect, and distanceZ.eyeSelect"
      )
      data <- data %>%
        dplyr::rowwise() %>%
        mutate(
          gazeX.eyeSelect := mean(c(
            .data$gpLeft.X.temp, .data$gpRight.X.temp
          ), na.rm = TRUE),
          gazeY.eyeSelect := mean(c(
            .data$gpLeft.Y.temp, .data$gpRight.Y.temp
          ), na.rm = TRUE),
          pupil.eyeSelect := mean(c(
            .data$pupilLeft.temp, .data$pupilRight.temp
          ), na.rm = TRUE),
          distanceZ.eyeSelect := mean(c(
            .data$distLeft.Z.temp, .data$distRight.Z.temp
          ), na.rm = TRUE)
        )
    }

    # Both Strict Option
    else if (eyeSelection_method == "Strict") {
      # Set eye selection parameter
      # Gaze
      data[["gazeX.es.selection"]] <- ifelse(
        !is.na(data[[gazeLeftX]]) & !is.na(data[[gazeRightX]]),
        "mean_strict",
        ifelse(
          !is.na(data[[gazeLeftX]]) & is.na(data[[gazeRightX]]),
          "right_na",
          ifelse(
            is.na(data[[gazeLeftX]]) & !is.na(data[[gazeRightX]]),
            "left_na",
            ifelse(is.na(data[[gazeLeftX]]) &
              is.na(data[[gazeRightX]]), "both_na",
            "other_strict"
            )
          )
        )
      )
      data[["gazeY.es.selection"]] <- ifelse(
        !is.na(data[[gazeLeftY]]) & !is.na(data[[gazeRightY]]),
        "mean_strict",
        ifelse(
          !is.na(data[[gazeLeftY]]) & is.na(data[[gazeRightY]]),
          "right_na",
          ifelse(
            is.na(data[[gazeLeftY]]) & !is.na(data[[gazeRightY]]),
            "left_na",
            ifelse(is.na(data[[gazeLeftY]]) &
              is.na(data[[gazeRightY]]), "both_na",
            "other_strict"
            )
          )
        )
      )
      # Pupils
      data[["pupil.es.selection"]] <- ifelse(
        !is.na(data[[pupilLeft]]) &
          !is.na(data[[pupilRight]]),
        "mean_strict",
        ifelse(
          !is.na(data[[pupilLeft]]) & is.na(data[[pupilRight]]),
          "right_na",
          ifelse(
            is.na(data[[pupilLeft]]) & !is.na(data[[pupilRight]]),
            "left_na",
            ifelse(is.na(data[[pupilLeft]]) &
              is.na(data[[pupilRight]]), "both_na",
            "other_strict"
            )
          )
        )
      )
      # Dist
      data[["distZ.es.selection"]] <- ifelse(
        !is.na(data[[distanceLeftZ]]) & !is.na(data[[distanceRightZ]]),
        "mean_strict",
        ifelse(
          !is.na(data[[distanceLeftZ]]) & is.na(data[[distanceRightZ]]),
          "right_na",
          ifelse(
            is.na(data[[distanceLeftZ]]) & !is.na(data[[distanceRightZ]]),
            "left_na",
            ifelse(is.na(data[[distanceLeftZ]]) &
              is.na(data[[distanceRightZ]]), "both_na",
            "other_strict"
            )
          )
        )
      )


      data <- data %>%
        dplyr::rowwise() %>%
        mutate(
          gazeX.eyeSelect := mean(c(
            !!rlang::sym(gazeLeftX), !!rlang::sym(gazeRightX)
          ), na.rm = FALSE),
          gazeY.eyeSelect := mean(c(
            !!rlang::sym(gazeLeftY), !!rlang::sym(gazeRightY)
          ), na.rm = FALSE),
          # gazeX.eyeSelect := ifelse(!! rlang::sym(gpLeft.X) == !! rlang::sym(p))
          pupil.eyeSelect := mean(c(
            !!rlang::sym(pupilLeft), !!rlang::sym(pupilRight)
          ), na.rm = FALSE),
          distanceZ.eyeSelect := mean(c(
            !!rlang::sym(distanceLeftZ), !!rlang::sym(distanceRightZ)
          ), na.rm = FALSE)
        )
    }

    # Single Eye Select Options
    else if (eyeSelection_method == "Left") {
      data[["gazeX.es.selection"]] <-
        case_when(
          !is.na(data[[gazeLeftX]]) ~ "left_only",
          is.na(data[[gazeLeftX]]) ~ "left_na",
          TRUE ~ "other"
        )
      data[["gazeY.es.selection"]] <-
        case_when(
          !is.na(data[[gazeLeftY]]) ~ "left_only",
          is.na(data[[gazeLeftY]]) ~ "left_na",
          TRUE ~ "other"
        )
      data[["pupil.es.selection"]] <-
        case_when(
          !is.na(data[[pupilLeft]]) ~ "left_only",
          is.na(data[[pupilLeft]]) ~ "left_na",
          TRUE ~ "other"
        )
      data[["distZ.es.selection"]] <-
        case_when(
          !is.na(data[[distanceLeftZ]]) ~ "left_only",
          is.na(data[[distanceLeftZ]]) ~ "left_na",
          TRUE ~ "other"
        )

      data <- data %>%
        dplyr::rowwise() %>%
        mutate(
          gazeX.eyeSelect = (!!rlang::sym(gazeLeftX)),
          gazeY.eyeSelect = (!!rlang::sym(gazeLeftY)),
          pupil.eyeSelect = (!!rlang::sym(pupilLeft)),
          distanceZ.eyeSelect = (!!rlang::sym(distanceLeftZ))
        )
    } else if (eyeSelection_method == "Right") {
      data[["gazeX.es.selection"]] <-
        case_when(
          !is.na(data[[gazeRightX]]) ~ "right_only",
          is.na(data[[gazeRightX]]) ~ "right_na",
          TRUE ~ "other"
        )
      data[["gazeY.es.selection"]] <-
        case_when(
          !is.na(data[[gazeRightY]]) ~ "right_only",
          is.na(data[[gazeRightY]]) ~ "right_na",
          TRUE ~ "other"
        )
      data[["pupil.es.selection"]] <-
        case_when(
          !is.na(data[[pupilRight]]) ~ "right_only",
          is.na(data[[pupilRight]]) ~ "right_na",
          TRUE ~ "other"
        )
      data[["distZ.es.selection"]] <-
        case_when(
          !is.na(data[[distanceRightZ]]) ~ "right_only",
          is.na(data[[distanceRightZ]]) ~ "right_na",
          TRUE ~ "other"
        )

      data <- data %>%
        rowwise() %>%
        mutate(
          gazeX.eyeSelect = (!!rlang::sym(gazeRightX)),
          gazeY.eyeSelect = (!!rlang::sym(gazeRightY)),
          pupil.eyeSelect = (!!rlang::sym(pupilRight)),
          distanceZ.eyeSelect = (!!rlang::sym(distanceRightZ))
        )
    }

    return(data)
  }
