#!/usr/bin/env lr

# Example: lr analyze.R --config_file="../config/analyze.yaml"

doc <- "
Process Activity Logs with Custom Configurations

Usage:
  script.R --config_file=<config_file>
  script.R (-h | --help)

Options:
  -h --help                       Show this screen.
  --config_file=<config_file>     Path to the YAML configuration file specifying input details, output preferences, and data processing instructions.

"

.libPaths(c("../library", .libPaths()))

box::use(
  docopt[docopt],
  yaml[read_yaml],
  glue[glue],
  purrr[pluck, chuck, imodify, map],
  stringr[str_ends],
  sessioninfo[session_info],
  jsonlite[to_json = toJSON],
  readr[write_lines],
  logger[log_trace, log_success, log_error],
  fs[file_exists],
  rlang[exec, is_null]
)


box::purge_cache()
options(box.path = "..")
box::use(R/logging)
box::use(R/analyze)
box::use(R/io)
options(box.path = NULL)

# Functions ---------------------------------------------------------------

load_config <- function(args, ...) {
  config_path <- chuck(.x = args, "--config_file")

  if (!file_exists(path = config_path)) {
    stop("Configuration file does not exist: ", config_path)
  }

  #  config_path <- "../config/process.yaml"
  read_yaml(file = config_path)
}

setup_logging <- function(settings, ...) {
  settings <- settings |>
    imodify(.f = \(val, key) if (str_ends(key, "file")) {
      glue(val, timestamp = format(Sys.time(), "%Y%m%d%H%M%S"))
    } else {
      val
    })

  exec(.fn = logging[["configure_logging"]], !!!settings)

  log_success("Completed configuring logging")

  return(settings)
}

analyze_data <- function(.config, .class, .f, ...) {
    structure(list(data = NULL, config = .config), class = .class) |>
    io[["read_data"]](input_folder = input_folder) |>
    analyze[[.f]]()  |>
    io[["write_data"]](output_folder = output_folder)
}

save_session_info <- function(logging_settings, ...) {
  file_path <- pluck(.x = logging_settings, "sessioninfo_file")

  if (!is_null(file_path)) {
    log_trace("Saving session_info to {file_path}")

    session_info(dependencies = TRUE) |>
      to_json(pretty = TRUE, auto_unbox = TRUE) |>
      write_lines(file = file_path)
  }
}

# Main Script ---------------------------------------------------------

try({
  # Parse command-line arguments
  args <- docopt(doc)

  config <- load_config(args = args)

  list2env(x = config, envir = .GlobalEnv)

  logging_settings <- setup_logging(settings = logging_settings)

  pluck(config, "time_bands", "exercises") |>
    analyze_data(.class = "exercises", .f = "add_time_bands")

  pluck(config, "time_bands", "meals") |>
    analyze_data(.class = "meals", .f = "add_time_bands")

  pluck(config, "moving_average", "weights") |>
    analyze_data(.class = "weights", .f = "add_moving_average")

  save_session_info(logging_settings = logging_settings)

}, silent = FALSE)
