##############
#' Find indices of fixations and saccades labelled by IVT filter
#'
#' @param entry.class a string specifying the classification column to use
#'
#' @return index summary - a list of fixation and saccade indexes, their start & end points
#' @export
#'
findFixationIndices <- function(entry.class) {
  rle_fix_index <- which(rle(entry.class)$values == "fixation")
  rle_sac_index <- which(rle(entry.class)$values == "saccade")
  rle_unc_index <-
    which(rle(entry.class)$values == "unclassified") # separate from missing data, it was collect but algorithmically thrown away

  rle_frag_index <- which(rle(!is.na(entry.class))$values == TRUE)
  rle_nofrag_index <-
    which(rle(!is.na(entry.class))$values == FALSE)

  end.sac <- cumsum(rle(entry.class)$lengths)
  start.sac <- c(1, lag(end.sac)[-1] + 1)
  lengths.sac <- end.sac - start.sac + 1

  end.unc <- cumsum(rle(entry.class)$lengths)
  start.unc <- c(1, lag(end.unc)[-1] + 1)
  lengths.unc <- end.unc - start.unc + 1

  end.fix <- cumsum(rle(entry.class)$lengths)
  start.fix <- c(1, lag(end.fix)[-1] + 1)
  lengths.fix <- end.fix - start.fix + 1

  end.frag <- cumsum(rle(!is.na(entry.class))$lengths)
  start.frag <- c(1, lag(end.frag)[-1] + 1)
  lengths.frag <- end.frag - start.frag + 1

  # print(data[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix],c("class","class.adj","class.adj.euc","class.adj.gap.dur","class.adj.xva","class.adj.yva","class.adj.num")])

  index_summary <-
    list(
      "rle_fix_index" = rle_fix_index,
      "rle_sac_index" = rle_sac_index,
      "rle_unc_index" = rle_unc_index,
      "rle_frag_index" = rle_frag_index,
      "rle_nofrag_index" = rle_nofrag_index,
      "end.fix" = end.fix,
      "start.fix" = start.fix,
      "lengths.fix" = lengths.fix,
      "end.sac" = end.sac,
      "start.sac" = start.sac,
      "lengths.sac" = lengths.sac,
      "end.unc" = end.unc,
      "start.unc" = start.unc,
      "lengths.unc" = lengths.unc,
      "end.frag" = end.frag,
      "start.frag" = start.frag,
      "lengths.frag" = lengths.frag
    )
  return(index_summary)
}
