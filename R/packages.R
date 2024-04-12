#' LOCAL LIBRARY

#' This script loads the packages saved in the latest session_info log in the
#' ../logs directory, extracts the list of package locations and copies the
#' package directories to the local ../library directory. This is to manage a self
#' contained and version controlled package repository. This script must be run
#' during development where the location of the packages is in the default
#' .libPaths() location.

# Install and load required packages using box
if (!requireNamespace("box", quietly = TRUE))
  install.packages("box")

box::use(fs[dir_info, path, dir_copy, path_join],
         dplyr[arrange, desc, pull, filter, distinct],
         stringr[str_detect, str_starts],
         readr[write_csv],
         purrr[pluck, map, map_df, compose],
         jsonlite[from_json = fromJSON])

LOGS_DIR <- path_join(c("..", "logs"))
LOCAL_LIB_PATH <- path_join(c("..", "library"))

sessioninfo_files <- dir_info(LOGS_DIR) |>
  filter(str_detect(path, "sessioninfo")) |>
  arrange(desc(modification_time)) |>
  pull(path)

packages <- sessioninfo_files |>
  map_df(.f = compose(\(x) pluck(x, "packages"), from_json)) |>
  distinct() |>
  filter(str_starts(path, .libPaths()[-1]))

write_csv(x = packages, file = path_join(c(LOCAL_LIB_PATH, "packages.csv")))

copied_packages <- packages |>
  pull(path) |>
  map(\(x) try(dir_copy(path = x, new_path = LOCAL_LIB_PATH))) |>
  unlist()
