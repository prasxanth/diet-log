.libPaths(c("../library", .libPaths()))

box::use(
  readr[write_csv],
  tibble[deframe],
  tsibble[as_tsibble],
  dplyr[nest_by, bind_rows],
  purrr[pluck, `pluck<-`, list_rbind],
  glue[glue],
  fs[path, path_ext],
  arrow[read_feather],
  rlang[exec],
  logger[log_info, log_trace, log_debug, log_success, log_error, log_fatal]
)

#' @export
read_data <- function(x, ...) {
  UseMethod("read_data")
}

#' @export
read_data.default <-
  function(input_file, input_folder, ...) {
    full_path <- path(input_folder, input_file)

    format <- path_ext(path = input_file)

    if (!(format %in% c("rds", "feather"))) {
      log_fatal("Input format must be 'rds' or 'feather' not {format}")
    }

    log_info("Reading data from {full_path}")

    tryCatch({
      data <- switch(
        format,
        rds = readRDS(file = full_path),
        feather = read_feather(file = full_path),
        log_error("Unsupported format: {format}")
      )

      log_success("Successfully read data from {full_path}; ")
      log_debug("Rows: {nrow(data)}; ",
                "Columns: |{data |> names() |> paste(collapse = ', ')}|")
      return(data)
    }, error = function(e) {
      log_error("Error reading log file: {full_path} - {e$message}")
      return(NULL)
    })
  }

#' @export
read_data.exercises <- function(obj, input_folder) {
  log_trace("Exercises data")
  input_file <- pluck(.x = obj, "config", "files", "input_file")
  pluck(.x = obj, "data") <- read_data(input_file, input_folder)
  return(obj)
}

#' @export
read_data.meals <- function(obj, input_folder) {
  input_file <- pluck(.x = obj, "config", "files", "input_file")
  pluck(.x = obj, "data") <-
    read_data(input_file, input_folder) |>
    nest_by(common_type, .keep = TRUE) |>
    deframe() |>
    as.list()
  return(obj)
}

#' @export
read_data.weights <- function(obj, input_folder) {
  input_file <- pluck(.x = obj, "config", "files", "input_file")
  pluck(.x = obj, "data") <- read_data(input_file, input_folder)
  return(obj)
}

#' @export
write_data <- function(x, ...) {
  UseMethod("write_data")
}

#' @export
write_data.default <- function(data, output_file, output_folder) {
  full_path <- path(output_folder, output_file)

  format <- path_ext(path = output_file)
  tryCatch({
    switch(
      format,
      rds = saveRDS(object = data, file = full_path),
      feather = write_feather(x = data, sink = full_path),
      csv = write_csv(x = data, file = full_path),
      log_error("Unsupported format: {format}")
    )
    log_success("✓ {output_file} data saved to: {full_path}")
  }, error = function(e) {
    log_error("Error saving {output_file} to {full_path}: {e$message}")
  })
}

#' @export
write_data.exercises <- function(obj, output_folder) {
  output_file <- pluck(.x = obj, "config", "files", "output_file")
  data <- pluck(.x = obj, "data")
  write_data(data, output_file, output_folder)
}

#' @export
write_data.meals <- function(obj, output_folder) {
  output_file <- pluck(.x = obj, "config", "files", "output_file")
  data <- pluck(.x = obj, "data") |> list_rbind()
  write_data(data, output_file, output_folder)
}

#' @export
write_data.weights <- function(obj, output_folder) {
  output_file <- pluck(.x = obj, "config", "files", "output_file")
  data <- pluck(.x = obj, "data")
  write_data(data, output_file, output_folder)
}
