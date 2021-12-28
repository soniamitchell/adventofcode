
# Read in cucumber positions
dat <- scan(text = "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>", what = "character") |>
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
camcorder::gg_record(dir = file.path(tempdir(),"recording"),
                     device = "png",
                     width = 2,
                     height = 2,
                     units = "in",
                     dpi = 300)

track_cucumbers$gg_cucumbers() |> print()

for (i in 1:58) {
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
