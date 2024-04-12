#!/usr/bin/env lr

# Example: lr process.R --config_file="../config/process.yaml"

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
  purrr[map, imodify, pluck, chuck, list_rbind, walk, modify_in],
  glue[glue],
  stringr[str_ends],
  yaml[read_yaml],
  rlang[exec, is_null],
  sessioninfo[session_info],
  jsonlite[to_json = toJSON],
  readr[write_lines],
  logger[log_trace, log_success, log_error],
  fs[file_exists]
)

options(box.path = "..")
box::use(R/logging)
box::use(R/process)
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

save_activities <-
  function(activities_params,
           data,
           output_folder,
           ...) {
    log_trace("Processing activities")

    walk(.x = activities_params, .f = \(params) {
      modify_in(.x = params,
                .where = list("separate_regex"),
                .f = process[["parse_separate_regex"]]) |>
        process[["process_activities"]](data = data) |>
        io[["write_data"]](output_file = chuck(.x = params, "output_file"),
                           output_folder = output_folder)
    })

    log_trace("Completed processing activities")
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

tryCatch({
  # Parse command-line arguments
  args <- docopt(doc)

  config <- load_config(args = args)

  list2env(x = config, envir = .GlobalEnv)

  logging_settings <- setup_logging(settings = logging_settings)

  data <-
    input_files |> map(.f = process[["read_log"]]) |> list_rbind()

  save_activities(
    activities_params = activities_params,
    data = data,
    output_folder = output_folder
  )

  save_session_info(logging_settings = logging_settings)
},
error = function(e) {
  log_error("Error encountered: {e$message}")
  stop("Error: {e$message}")
})

# cat ../logs/process_20240329181901.log | jq -r '[.level, .time, .msg] | @tsv' | tr -d '"[]' | column -s -t
# mlr --ijson --opprint cut -f level,time,msg ../logs/process_20240329200604.log
