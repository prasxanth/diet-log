.libPaths(c("../library", .libPaths()))

box::use(
  logger[log_trace, log_info, log_debug, log_eval, log_success, log_error],
  glue[glue],
  readr[parse_time],
  dplyr[mutate, distinct, pick, case_match, select, left_join, pull, first],
  magrittr[`%>%`, add],
  stats[median],
  tidyr[replace_na],
  tibble[tibble],
  lubridate[seconds_to_period, period_to_seconds, hms, hour, minute, second],
  purrr[map, map_df, pmap, compose, pluck, `pluck<-`, list_rbind],
  slider[slide_mean],
  rlang[is_null, list2, exec, `%||%`]
)

# Function to convert period to time format
#' @export
period_to_time <- function(period) {
  period |>
    {
      \(x) sprintf("%02i:%02i:%02i", hour(x), minute(x), second(x))
    }() |>
    parse_time()
}

hms_to_seconds <- compose(period_to_seconds, hms)

time_arithmetic <- function(fn, ...) {
  # Collect arguments and convert them to seconds.
  args <- list(...)
  args_in_seconds <- map(args, hms_to_seconds)

  # Attempt to call the function with the arguments as a list.
  result <- tryCatch({
    exec(fn, args_in_seconds)
  }, error = function(e) {
    # If the function fails to execute, assume it requires individual arguments.
    exec(fn, !!!args_in_seconds)
  })

  # Convert the result back to a period and then to a time format.
  result |> seconds_to_period() |> period_to_time()
}

# Function to add time bands to a dataframe
#' @export
add_time_bands <- function(x, ...) {
  UseMethod("add_time_bands")
}

#' @export
add_time_bands.default <-
  function(df,
           median_default = NULL,
           ucl_offset = "00:45:00",
           lcl_offset = "00:45:00",
           ...) {
    data <- df |>
      mutate(
        time_median = median_default %||% time_arithmetic(fn = median, time),
        time_ucl = time_arithmetic(fn = `+`, time_median, ucl_offset),
        time_lcl = time_arithmetic(fn = `-`, time_median, lcl_offset)
      )

    log_success("Added time bands",
                "Median: {data |> pull(time_median) |> first()}",
                "UCL Offset: {ucl_offset}",
                "LCL Offset: {lcl_offset}")

    return(data)
  }

#' @export
add_time_bands.exercises <-
  function(obj, ...) {
    log_info("Adding time bands to exercises")

    data <- pluck(obj, "data")
    offsets <- pluck(obj, "offsets")

    args <- list2(df = data, !!!offsets)

    pluck(obj, "data") <- do.call(add_time_bands, args)

    return(obj)
  }

#' @export
add_time_bands.meals <-
  function(obj, ...) {
    log_info("Adding time bands to meals")

    data <- pluck(obj, "data")
    config <- pluck(obj, "config")

    main_meals <- c("breakfast", "lunch", "dinner")

    pluck(obj, "data")[main_meals] <-
      map(main_meals,
          \(meal) {
            log_info("Adding time bands to meal: { meal }")
            exec(add_time_bands, df = pluck(data, meal), !!!pluck(config, meal))
          })

    time_bands_lookup <- pluck(obj, "data")[main_meals] |>
      list_rbind() |>
      distinct(pick(common_type, time_median, time_ucl, time_lcl)) %>%
      split(.$common_type) |>
      map(\(x) x |> select(-common_type) |> as.list())

    snack_times <- grep("snacks", names(config), value = TRUE)
    snacks_config <- config[snack_times]

    pluck(obj, "data")[snack_times] <-
      calculate_snacks_time_bands_params(snacks_config = snacks_config,
                                         time_bands_lookup = time_bands_lookup) |>
      pmap(\(snack_time, ...) {
        log_info("Adding time bands to snacks: { snack_time }")
        add_time_bands(pluck(data, snack_time), ...)
      })

    return(obj)
  }

calculate_snacks_time_bands_params <-
  function(snacks_config, time_bands_lookup, ...) {
    log_info("Calculating parameters for snacks time bands")

    data <- map_df(
      snacks_config,
      \(x) tibble(
        median_start = pluck(
          time_bands_lookup,
          !!!pluck(x, "median", "start"),
          .default = pluck(x, "median", "start")
        ) |> as.character(),
        median_stop = pluck(
          time_bands_lookup,
          !!!pluck(x, "median", "stop"),
          .default = pluck(x, "median", "stop")
        ) |> as.character(),
        ucl_offset = pluck(x, "ucl_offset"),
        lcl_offset = pluck(x, "lcl_offset")
      ),
      .id = "snack_time"
    ) |>
      mutate(
        median = case_match(
          snack_time,
          "snacks_morning" ~ time_arithmetic(fn = median, c(hms(median_start), hms(median_stop))),
          "snacks_afternoon" ~ time_arithmetic(fn = `+`, median_stop, median_start),
          "snacks_evening" ~ time_arithmetic(fn = `-`, median_stop, median_start)
        )
      ) |>
      select(-median_start, -median_stop) |>
      suppressWarnings()

    log_success("Completed calculating snacks time bands parameters")

    return(data)
  }

#' @export
add_moving_average <- function(x, ...) {
  UseMethod("add_moving_average")
}

#' @export
add_moving_average.weights <- function(obj, ...) {
  winsize <-
    pluck(obj, 'config', 'window_size') |> unlist() |> sum() |> add(1)

  log_info("Adding { winsize } day moving average column")

  data <- pluck(obj, "data")

  pluck(obj, "data") <- tibble(date = seq(
    from = min(data[["date"]]),
    to = max(data[["date"]]),
    by = "day"
  )) |>
    left_join(data, by = "date") |>
    mutate(
      time = replace_na(time, parse_time("09:00")),
      weight_3day_average = slide_mean(
        weight_lbs, !!!pluck(obj, "config", "window_size"),
        na_rm = TRUE
      )
    )

  log_success("Completed adding { winsize } day moving average column")

  return(obj)
}
