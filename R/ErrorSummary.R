#' Error Summary
#'
#' Summarizes all errors in a DataVolley play-by-play file
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#' @param ... Variables to group by.
#'
#' @return A grouped data frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom dplyr full_join
#' @importFrom tidyselect ends_with
#' @importFrom rlang .data
#' @importFrom tidyr replace_na
#'
#' @export
ErrorSummary <- function(plays, ...){

  touch_types <- c("Serve", "Reception", "Attack", "Block", "Dig", "Set", "Freeball")

  errors_list <- vector("list", length = 7)

  for(i in 1:7){
    error_df <- plays %>%
      filter(skill == touch_types[i]) %>%
      group_by(...) %>%
      summarize(Attempts = n(),
                Errors = Errors(.data$evaluation) + AttackStuffs(.data$evaluation),
                `Error%` = .data$Errors/.data$Attempts)
    df_cols <- c("Attempts", "Errors", "Error%")
    names(df_cols) <- paste(touch_types[i], c("Attempts","Errors","Error%"), sep = "")
    errors_list[[i]] <- select(error_df, ..., !!!df_cols)
  }

  errors_output_replace <- rep(list(0), 14) ## 14 0's
  names(errors_output_replace) <- paste(rep(touch_types, each = 2), c("Attempts","Errors"), sep = "")

  errors_output <- suppressMessages(Reduce(full_join, errors_list)) %>%
    replace_na(errors_output_replace) %>% ungroup()

  # Note: we have to ungroup first to do the mutate because otherwise the grouping variables show up in the tidyselector
  output <- errors_output %>%
    mutate(TotalAttempts = apply(select(errors_output, ends_with("Attempts")), 1, sum),
           TotalErrors = apply(select(errors_output, ends_with("Errors")), 1, sum),
           ErrorPoints = .data$TotalErrors - .data$BlockErrors - .data$DigErrors, # block and dig errors do not usually "lose" the point
           `TotalError%` = .data$TotalErrors/.data$TotalAttempts) %>%
    group_by(...)


  return(output)

}

