library(dplyr)
library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(tidyr)
library(purrr)


# Create the connection to a database and "studies" and "sponsors" tables.
con = dbConnect(
  duckdb(
    file.path("ctrialsgov.duckdb"), 
    read_only = TRUE
  )
)

if (length(dbListTables(con)) == 0) {
  stop("Problem reading from connection.")
}

studies = tbl(con, "studies")
sponsors = tbl(con, "sponsors")
outcomes = tbl(con, "outcomes")

#' @title Query keywords from a database table.
#' @description Description goes here.
#' @param d the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords 
#' (intersection) or any of the keywords (union)? (default FALSE; union).
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query)) 
}


# Create a histogram of the phases returned by a brief title keyword search
# @param d the database table.
# @param brief_title_kw the brief title keywords to look for. This is optional.
all_phases <- c("Early Phase 1", "NA", "Not Applicable", "Phase 1", "Phase 1/Phase 2", "Phase 2","Phase 2/Phase 3", "Phase 3", "Phase 4")
plot_phase_histogram = function(studies_df) {
  studies_df <- studies_df %>%
    mutate(phase = factor(phase, levels = all_phases)) %>%
    count(phase) %>%
    complete(phase = all_phases, fill = list(n = 0)) # Ensure all phases are included
  
  ggplot(studies_df, aes(x = phase, y = n)) +
    geom_col() +
    scale_x_discrete(limits = all_phases) + # Make sure the x-axis is uniform
    theme_bw() +
    xlab("Phase") +
    ylab("Count") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) # Rotate the x-axis labels for better legibility
}


#' Get the number of concurrent trials for each date in a set of studies
#' @param d the studies to get the number of concurrent trials for.
#' @return A tibble with a `date` column and a `count` of the number of
#' concurrent trials at that date.
get_concurrent_trials = function(d) {
  
  #browser()
  # Get all of the unique dates.
  all_dates = d |> 
    pivot_longer(cols = everything()) |>
    select(-name) |>
    distinct() |> 
    arrange(value) |>
    na.omit() |> 
    rename(date = value)
  
  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }
  
  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count = 
    map_dbl(
      all_dates$date, 
      ~ .x |> 
        within_date(d$start_date, d$completion_date) |>
        sum(na.rm = TRUE)
    )
  return(all_dates)
}


#' Plot the concurrent trials for each date in a set of studies
#' @param studies the studies to plot concurrent trials for.
plot_concurrent_studies = function(studies) {
  plot(mtcars$mpg, mtcars$cyl)
}

