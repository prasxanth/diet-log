.libPaths(c("../library", .libPaths()))

box::use(
  logger[get_logger_meta_variables,
         fail_on_missing_package,
         log_trace,
         log_debug,
         log_info,
         log_error,
         log_layout,
         log_formatter,
         formatter_json,
         log_appender,
         appender_console,
         appender_void,
         appender_tee,
         appender_file,
         log_threshold,
         TRACE],
   glue[glue],
   purrr[map],
   vctrs[vec_cast],
   rlang[exec, expr, list2, is_null, is_false, englue],
   jsonlite[to_json = toJSON]
)

layout_json <- function(fields = c('time', 'level', 'msg')) {

    force(fields)

    structure(function(level,
                       msg,
                       namespace = NA_character_,
                       .logcall = sys.call(),
                       .topcall = sys.call(-1),
                       .topenv = parent.frame()) {
      fail_on_missing_package('jsonlite')

      json <- get_logger_meta_variables(
        log_level = level,
        namespace = namespace,
        .logcall = .logcall,
        .topcall = .topcall,
        .topenv = .topenv
      )

      sapply(msg, function(msg)
        to_json(c(json, list(msg = msg))[fields], pretty = TRUE, auto_unbox = TRUE))

    }, generator = deparse(match.call()))
}

formatter_json <-
  structure(function(...,
                     .logcall = sys.call(),
                     .topcall = sys.call(-1),
                     .topenv = parent.frame()) {
    fail_on_missing_package('jsonlite')

    formatted_args <- list2(...)
    glued_exprs <- map(formatted_args, \(x) expr(glue(!!x)))
    glued_strings <- map(glued_exprs, \(x) eval(x, envir = .topenv))

    to_json(glued_strings, auto_unbox = TRUE)
  }, generator = quote(formatter_json()))

#' @export
configure_logging <- function(log_file = NULL,
                              console = FALSE,
                              layout_fields = NULL,
                              threshold = "TRACE",
                              ...) {
  # header <- paste(layout_fields, collapse = " | ")
  # layout_format <- layout_fields |>
  #   purrr::map(\(x) paste0("{", x, "}")) |> paste(collapse = " | ")
  # logger::log_layout(layout = logger::layout_glue_generator(format = layout_format))

  if (!is_null(layout_fields)) {
    log_layout(layout = layout_json(fields = layout_fields))
  } else {
    log_layout(layout = layout_json())
  }

  log_formatter(formatter_json)
  log_threshold(get(threshold, envir = asNamespace("logger")))

  if (is_null(log_file) && is_false(console)) {
    # Disable logging
    log_appender(appender = appender_void)
  } else if (!is_null(log_file)) {
    if (console) {
      # Log both to console and file
      log_appender(appender = appender_tee(log_file))
    } else {
      # Log only to file
      log_appender(appender = appender_file(log_file))
    }
  } else if (console) {
    # Log only to console
    log_appender(appender = appender_console)
  }
}

#' @export
pipe_log <- function(x, log_level, msg, ...) {
  # Pipeable version of logging functions
  # eg: list(a = 1, b = 2) |> log_msg(debug, "This is a debug message")
  force(x)
  exec(englue("log_{{ log_level }}"), msg, ...)
  return(x)
}
