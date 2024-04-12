.libPaths(c("../library", .libPaths()))

box::use(
  logger[log_trace, log_info, log_debug, log_success, log_error],
  glue[glue],
  readr[read_csv, cols, col_datetime, parse_time, type_convert],
  dplyr[rename_with, mutate, select, filter],
  tidyr[separate_wider_regex],
  stringr[str_replace_all, str_split],
  purrr[map, pluck],
  rlang[set_names, is_null, `%||%`]
)

#' @export
read_log <- function(path) {
  log_info("Reading log file: {path}")

  # Attempt to read the CSV file with specific parsing and transformation
  tryCatch({
    data <- read_csv(file = path,
                     col_types = cols(Datetime = col_datetime(format = "%m/%d/%Y %H:%M:%S"))) |>
      rename_with(.fn = tolower) |>
      mutate(
        date = as.Date(datetime),
        time = format(datetime, "%H:%M:%S") |> parse_time(),
        activity = tolower(activity)
      )

    log_success(
      "Successfully read log file: {path}; ",
      "Rows: {nrow(data)}; ",
      "Columns: |{data |> names() |> paste(collapse = ', ')}|"
    )
    return(data)
  }, error = function(e) {
    # Log an error if the file reading fails
    log_error("Error reading log file: {path} - {e$message}")
    return(NULL)
  })
}

# Applies multiple string replacements sequentially to a given string, using corresponding pairs of patterns and replacements.
multi_str_replace_all <- function(string, pattern, replacement) {
  if (length(pattern) != length(replacement)) {
    stop("Pattern and replacement lengths differ.")
  }

  for (i in seq_along(pattern)) {
    string <- str_replace_all(string = string,
                              pattern = pattern[i],
                              replacement = replacement[i])
  }

  return(string)
}

# Convert the vector of strings from separate_regex yaml entry into a list with named and unnamed elements
#' @export
parse_separate_regex <- function(regex_list) {
  map(.x = regex_list, .f = \(x) {
    parts <- str_split(string = x,
                       pattern = "=",
                       simplify = TRUE)
    if (length(parts) == 2)
      set_names(x = list(parts[2]), nm = parts[1])
    else
      list(x)
  }) |>
    unlist()
}

#' This function processes activities data by filtering based on activity type,
#' separating wide data based on regex patterns, and converting column data types.
#' @export
process_activities <- function(data, activity_config) {
  activity_type <- pluck(.x = activity_config, "activity_type")
  separate_regex <- pluck(.x = activity_config, "separate_regex")
  col_types <- pluck(.x = activity_config, "col_types")

  log_info("Processing activities of type: {activity_type}")

  processed_data <- data |>
    filter(activity == activity_type) |>
    separate_wider_regex(cols = value, patterns = separate_regex) |>
    select(-activity, -datetime) |>
    type_convert(col_types = col_types, guess_integer = TRUE) |>
    suppressMessages()

  log_success(
    "Processed and cleaned data; ",
    "Rows: {nrow(processed_data)}; ",
    "Columns: |{processed_data |> names() |> paste0(collapse = '; ')}|"
  )

  mutations <- pluck(.x = activity_config, "mutations")

  # Check if there are mutations to apply
  if (!is_null(mutations)) {
    log_info("Applying mutations for activity type: {activity_type}")

    for (mutation in mutations) {
      source_col <- pluck(.x = mutation, "source")
      transformation <- pluck(.x = mutation, "transformation")

      additional_args <- pluck(.x = mutation, "additional_args")
      col_name <- pluck(.x = mutation, "name")

      # Log the specific mutation being applied
      log_info(
        "Applying transformation '{transformation}' to column '{source_col}' for activity type: {activity_type}"
      )

      # Initialize additional_args if not provided
      supplemental_args <- additional_args %||% list()

      # Dynamically build the transformation function call
      args_list <- c(as.symbol(source_col), supplemental_args)

      # Dynamically apply the transformation function
      processed_data <- processed_data |>
        mutate(!!col_name := do.call(transformation, args_list))
    }
  } else {
    log_info("No mutations defined for activity type: {activity_type}")
  }

  log_success("Completed processing activities of type: {activity_type}")
  return(processed_data)
}
