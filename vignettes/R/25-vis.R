# Read in cucumber positions
dat <- readLines(here("inst", "2021", "day25-test.txt")) |>
  strsplit("") |>
  do.call(what = rbind)

east_cucumbers <- which(dat == ">", arr.ind = TRUE) |>
  data.frame() |>
  dplyr::mutate(type = "east")
south_cucumbers <- which(dat == "v", arr.ind = TRUE) |>
  data.frame() |>
  dplyr::mutate(type = "south")
cucumbers <- rbind(east_cucumbers, south_cucumbers) |>
  dplyr::mutate_if(is.numeric, as.double)

# Start visualisation -----------------------------------------------------

track_cucumbers <- sea_cucumbers$new(dat, cucumbers)

# Initialise recorder
camcorder::gg_record(dir = "recording_tmp",
                     device = "png",
                     width = 2,        # 12 for the full dataset
                     height = 2,       # 12 for the full dataset
                     units = "in",
                     dpi = 300)

track_cucumbers$gg_cucumbers() |> print()

for (i in 1:58) {                      # 530 for the test dataset
  track_cucumbers$move_east()
  track_cucumbers$move_south()
  track_cucumbers$gg_cucumbers() |> print()
}

# Generate gif
camcorder::gg_playback(
  name = file.path("recording.gif"),
  first_image_duration = 8,
  last_image_duration = 12,
  frame_duration = .25,
  background = "none"
)
