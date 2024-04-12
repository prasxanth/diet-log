.libPaths(c("../library", .libPaths()))

box::use(logger[log_trace, log_info, log_debug, log_eval, log_error],
         glue[glue],
         rlang[exec, is_list, is_character],
         purrr[pluck, map],
         dplyr[distinct],
         readr[parse_time],
         lubridate[days],
         ggplot2[aes,
                 theme,
                 element_blank,
                 element_text,
                 element_rect,
                 element_line,
                 geom_hline,
                 geom_ribbon,
                 geom_point,
                 geom_smooth,
                 geom_text,
                 scale_color_manual,
                 scale_fill_manual,
                 scale_shape_manual],
         plotly[ggplotly, layout],
         utils[modify_list = modifyList]
)

# Function to convert a list of theme settings to a ggplot theme
#' @export
create_ggplot_theme <- function(settings) {
  settings_list <- map(settings, function(setting_value) {
    if (!is.list(setting_value)) {
      return(setting_value)
    }

    element_fun <- pluck(names(setting_value), 1)
    element_args <- pluck(setting_value, element_fun)

    # Dynamically call the element function
    exec(element_fun, !!!element_args)
  })

  exec(theme, !!!settings_list)
}

#' @export
substitute_theme_colors <- function(settings, palette) {
  # Function to recursively process each item
  process_item <- function(item) {
    if (is.list(item)) {
      # change to is_list
      # Recursively process each element of the list
      return(map(item, process_item))
    } else if (is.character(item)) {
      # Use glue for string interpolation and return the result
      interpolated <-
        glue(item, .envir = palette) |> as.character()
      return(interpolated)
    } else {
      # Return non-character items unchanged
      return(item)
    }
  }

  # Process the initial settings list
  processed_settings <- process_item(settings)
  return(processed_settings)
}

geom_time_median <- function(.data, .aes = aes(), ...) {
  geom_hline(
    data = .data,
    mapping = modify_list(aes(yintercept = time_median), .aes),
    linetype = 2,
    alpha = 0.5,
    ...
  )
}

geom_time_bands <- function(.data, .aes = aes(), ...) {
  geom_ribbon(data = .data,
              mapping = modify_list(aes(
                x = date,
                y = time,
                ymin = time_lcl,
                ymax = time_ucl
              ), .aes),
              alpha = 0.10,
              ...)
}

geom_scatter <- function(.data, .aes = aes(), ...) {
  geom_point(data = .data,
             mapping = modify_list(aes(x = date, y = time), .aes),
             ...)
}

geom_annotate <- function(.data, .aes = aes(), ...) {
  geom_text(
    data = .data |> distinct(category, time_median, max_date),
    mapping = modify_list(
      aes(
        x = max_date + days(3),
        y = time_median + parse_time("00:15:00"),
        label = category
      ),
      .aes
    ),
    size = 5,
    ...
  )
}

#' @export
geom_meals <- function(.data, .color_palette) {
  layers <- list(
    geom_time_median(.data = .data, .aes = aes(color = category)),
    geom_time_bands(.data = .data, .aes = aes(fill = category)),
    scale_fill_manual(values = .color_palette),
    geom_scatter(
      .data = .data,
      .aes = aes(
        color = category,
        shape = category,
        text = glue("{category}: {description}\nDate: {date}\nTime: {time}")
      ),
      size = 5
    ),
    scale_color_manual(values = .color_palette),
    scale_shape_manual(values = c(1, 21, 22, 23, 24, 25)),
    geom_annotate(.data = .data, .aes = aes(color = category))
  )

  return(layers)
}

#' @export
geom_exercises <- function(.data, .color = "#94e2d5") {
  layers <- list(
    geom_time_median(.data = .data, color = .color),
    geom_time_bands(.data = .data, fill = .color),
    geom_scatter(
      .data = .data,
      .aes = aes(
        size = calories_burned,
        text = glue(
          "Calories Burned: {calories_burned}\nDate: {date}\nTime: {time}"
        )
      ),
      color = .color
    ),
    geom_annotate(.data = .data, color = .color)
  )

  return(layers)
}

#' @export
geom_weights <- function(.data, ...) {
  layers <- list(
    geom_point(
      data = .data,
      aes(
        x = date,
        y = weight_lbs,
        text = glue("Weight: {weight_lbs}\nDate: {date}")
      ),
      size = 5,
      color = "#f9e2af"
    ),
    geom_smooth(
      data = .data,
      aes(x = date, y = weight_3day_average),
      linewidth = 1,
      color = "#f9e2af",
      method = "loess"
    )
  )

  return(layers)
}

#' @export
plotlify <- function(..., tooltip = NULL, background_color) {
  ... |>
    ggplotly(tooltip = tooltip) |>
    layout(paper_bgcolor = background_color,
           plot_bgcolor = background_color)
}
