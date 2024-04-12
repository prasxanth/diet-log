#!/usr/bin/env lr

# Example: lr plot.R --config_file="../config/plot.yaml"

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
  dplyr[mutate, pull, distinct],
  tidyr[nesting, complete],
  purrr[pluck, `pluck<-`, chuck, imodify, map, map_chr],
  forcats[as_factor],
  stringr[str_ends, str_replace_all, str_to_title, str_split, str_flatten],
  sessioninfo[session_info],
  jsonlite[to_json = toJSON],
  readr[write_lines],
  logger[log_trace, log_error],
  fs[file_exists, path],
  rlang[exec, is_null],
  ggplot2[theme, theme_minimal, element_blank, ggplot, labs, xlim, ylim],
  plotly[ggplotly, layout, subplot],
  utils[modify_list = modifyList],
  lubridate[days],
  logger[log_trace, log_info, log_success, log_debug, log_error],
  magrittr[`%>%`],
  htmlwidgets[save_widget = saveWidget]
)

box::purge_cache()
options(box.path = "..")
box::use(R/logging)
box::use(R/logging[pipe_log])
box::use(diet_plot = R/plot)
box::use(R/io)
options(box.path = NULL)

# Functions ---------------------------------------------------------------

load_config <- function(args, ...) {
  config_path <- chuck(.x = args, "--config_file")
  # config_path <- "../config/plot.yaml"

  if (!file_exists(path = config_path)) {
    stop("Configuration file does not exist: ", config_path)
  }

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

read_analysis <- function(x, ...) {
  UseMethod("read_analysis")
}

read_analysis.meals <- function(obj, ...) {
  pluck(obj, "data") <-
    io[["read_data"]](
      input_file = pluck(obj, "input_file"),
      input_folder = pluck(obj, "input_folder")
    ) |>
    pipe_log(debug, "Turning implicit missing values into explicit missing values") |>
    complete(
      date,
      nesting(common_type, time_median, time_ucl, time_lcl),
      fill = list(description = NA, time = NA),
      explicit = FALSE
    )

  log_success("Finished reading and preprocessing meals analysis data")

  return(obj)
}

read_analysis.exercises <- function(obj, ...) {
  pluck(obj, "data") <-
    io[["read_data"]](
      input_file = pluck(obj, "input_file"),
      input_folder = pluck(obj, "input_folder")
    )
  return(obj)
}

add_plot_info <- function(x, ...) {
  UseMethod("add_plot_info")
}

add_plot_info.meals <- function(obj, ...) {
  pluck(obj, "data") <- pluck(obj, "data") |>
    mutate(
      category = common_type |>
        str_replace_all("_", " ") |>
        str_to_title() |>
        str_split("\\b\\s+") |>
        map(rev) |>
        map_chr(\(x) str_flatten(x, collapse = " ")) |>
        as_factor(),
      max_date = max(date)
    )
  return(obj)
}

add_plot_info.exercises <- function(obj, max_date, ...) {
  pluck(obj, "data") <- pluck(obj, "data") |>
    mutate(category = "Exercise", max_date = max_date)
  return(obj)
}

save_session_info <- function(logging_settings, ...) {
  file_path <- pluck(.x = logging_settings, "sessioninfo_file")

  if (!is_null(file_path)) {
    log_info("Saving session_info to {file_path}")

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

  meals <-
    structure(list(
      input_file = pluck(config, "files", "input", "meals"),
      input_folder = input_folder,
      data = NULL
    ),
    class = "meals") |>
    read_analysis() |>
    add_plot_info() |>
    pluck("data")

  exercises <-
    structure(list(
      input_file = pluck(config, "files", "input", "exercises"),
      input_folder = input_folder,
      data = NULL
    ),
    class = "exercises") |>
    read_analysis() |>
    add_plot_info(max_date = meals |> distinct(max_date) |> pull()) |>
    pluck("data")

  weights <- pluck(config, "files", "input", "weights") |>
    io[["read_data"]](input_folder)

  ggtheme_diet <-
    theme_minimal(base_family = "Fira Sans Condensed Light", base_size = 16) +
    (
      pluck(config, "theme") |>
        diet_plot[["substitute_theme_colors"]](
          palette = pluck(config, "palette", "catpuccin_mocha", "canvas")
        ) |>
        diet_plot[["create_ggplot_theme"]]()
    )

  catpuccin_mocha <- pluck(config, "palette", "catpuccin_mocha", "main")
  background_color <- pluck(config, "palette", "catpuccin_mocha", "canvas", "background")

  ggplotly_activities <-
    (
      ggplot() +
        diet_plot[["geom_meals"]](.data = meals, .color_palette = catpuccin_mocha) +
        diet_plot[["geom_exercises"]](.data = exercises) +
        labs(x = "\nDay of Month", y = "Time (HH:MM:SS)\n") +
        xlim(min(meals |> pull(date)),
             max(meals |> pull(date)) + days(4)) +
        ggtheme_diet +
        theme(legend.position = "none")
    ) |>
    diet_plot[["plotlify"]](tooltip = c("text"), background_color = background_color)

  ggplotly_weights <-
    (
      ggplot() +
        diet_plot[["geom_weights"]](.data = weights) +
        xlim(min(meals |> pull(date)),
             max(meals |> pull(date)) + days(4)) +
        ylim(164, 174) +
        labs(x = "\nDay of Month", y = "Weight (lbs)\n\n") +
        ggtheme_diet +
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank())
    ) |>
    diet_plot[["plotlify"]](tooltip = c("text"), background_color = background_color)

  plotly_title <-
    list(
      text = "Approximately 4 lbs Weight Loss Over Six Weeks with Change in Diet",
      y = 0.99,
      yanchor = "top",
      font = list(
        size = 30,
        color = pluck(config, "palette", "catpuccin_mocha", "canvas", "text"),
        margin = list(
          t = 200,
          b = 40,
          l = 40,
          r = 40
        )
      )
    )

  combined_plot <- subplot(
    ggplotly_weights,
    ggplotly_activities,
    nrows = 2,
    heights = c(0.2, 0.8),
    shareX = TRUE,
    titleY = TRUE
    ) |>
    layout(title = plotly_title)

  # Display the combined interactive plot
  save_widget(widget = combined_plot,
              file = path(output_folder, pluck(config, "files", "output")))

  save_session_info(logging_settings = logging_settings)

}, silent = FALSE)
