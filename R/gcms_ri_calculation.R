#' Title
#'
#' @param sample_rt
#' @param alkane_rt
#'
#' @return
#' @export
#'
#' @examples
ri_calculation <- function(sample_rt, alkane_rt) {
  sort_table <- alkane_rt %>%
    add_row(instrument_rt = sample_rt) %>%
    arrange(instrument_rt) %>%
    pull(which(is.na(ki)))

  samp_pos <- pull(which(is.na(sort_table$ki)))

  n <- alkane_rt[samp_pos-1, 1]
  tn <- alkane_rt[samp_pos-1, 3]
  tN <- alkane_rt[samp_pos, 3]

  ri_value <- 100*(n+((sample_rt - tn)/(tN - tn)))

  ri_value <- round(ri_value)

  return(ri_value)
}
