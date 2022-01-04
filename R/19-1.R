#' Day 19: Beacon Scanner
#' @source <https://adventofcode.com/2021/day/19>
#' @name day19
#'
NULL

#' @rdname day19
#' @param path file path
#' @export
#'
read_day19 <- function(path) {
  dat <- path |>
    readLines() |>
    data.frame() |>
    setNames("data")

  start <- grep("scanner", dat$data)
  end <- c(tail(start - 1, -1), nrow(dat))

  lapply(seq_along(start), function(x) {
    tmp <- dat[start[x]:end[x], ]
    tmp <- tmp[-1]
    index <- which(tmp == "")
    if (length(index) != 0)
      tmp <- tmp[-index]
    out <- data.frame(data = tmp) |>
      tidyr::separate(col = data, into = c("d1", "d2", "d3"), sep = ",") |>
      dplyr::mutate_if(is.character, as.numeric) |>
      as.matrix()
    colnames(out) <- NULL
    out
  })
}

#' @rdname day19
#' @param scans scans
#' @export
#'
assemble_map <- function(scans) {
  transformations <- get_combinations(1:3)
  scanners <- data.frame(scanner = 0, x = 0, y = 0, z = 0)

  while(nrow(scanners) != length(scans)) {
    results <- list()

    # Compare each scanner output to that of scanner 0
    for (i in tail(seq_along(scans), -1)) {

      # cat("\r", i, "of", length(scans), "-",
      #     length(scans) - nrow(scanners), "left to find...")

      if ((i - 1) %in% scanners$scanner) next

      scanner_zero <- scans[[1]]
      scanner_i <- scans[[i]]

      for (j in seq_len(nrow(transformations))) {
        this_transformation <- transformations[j, ]

        # Transform `scanner_i` by `this_transformation`
        transformed_scanner <- transform_all(scanner_i, this_transformation)

        # Try subtracting each beacon coordinate in `scanner_zero` from each
        # beacon coordinate in `transformed_scanner` (transformation of
        # `scanner_i`)
        subtract <- lapply(seq_len(nrow(scanner_zero)), function(x)
          apply(transformed_scanner, 1, function(y) y - scanner_zero[x, ]) |>
            t()) |>
          do.call(what = rbind.data.frame) |>
          tidyr::unite(unscramble) |>
          dplyr::group_by(unscramble) |>
          dplyr::summarize(n = dplyr::n())

        # If 12 or more matches are found, record the scanner position
        if (max(subtract$n) >= 12) {
          this_coordinate <- subtract |>
            dplyr::filter(n == max(n)) |>
            tidyr::separate(unscramble, c("x", "y", "z"), sep = "_") |>
            dplyr::select(-n) |>
            data.frame() |>
            dplyr::mutate_if(is.character, as.numeric)

          colnames(this_coordinate) <- c("x", "y", "z")

          scanners <- rbind(scanners,
                            cbind(scanner = i - 1, this_coordinate))
          results[[i]] <- list(scanner1 = 0,
                               scanner2 = i - 1,
                               beacon = this_coordinate,
                               transformation = this_transformation)
          break
        }
      }
    }

    # Which beacons can see each other?
    relatives <- lapply(seq_along(results), function(x)
      if (is.null(results[[x]])) {
        NA
      } else {
        data.frame(from = results[[x]]$scanner1,
                   to = results[[x]]$scanner2)
      }
    ) |>
      do.call(what = rbind)

    # Append new beacon coordinates to scanner outputs
    for (k in seq_len(nrow(relatives))) {
      if (all(is.na(relatives[k,]))) next

      tmp <- results[[k]]
      from <- relatives$from[k] + 1
      to <- relatives$to[k] + 1

      # Transform `to` scanner coordinates relative to `from` scanner coordinates
      transformed_to <- transform_all(scans[[to]], tmp$transformation) |>
        apply(1, function(x) x - tmp$beacon) |>
        do.call(what = rbind)
      colnames(transformed_to) <- c("X1", "X2", "X3")

      # Add beacon coordinates to `from` scanner
      scans[[from]] <- transformed_to |>
        dplyr::anti_join(data.frame(scans[[from]]),
                         by = c("X1", "X2", "X3")) |>
        as.matrix() |>
        rbind(scans[[from]]) |>
        unique()
    }
  }

  list(scanners = scanners, scans = scans)

}

get_combinations <- function(x) {
  x |> spin_and_flip() |>
    reorient() |>
    spin_and_flip() |>
    reorient() |>
    spin_and_flip()
}

spin_and_flip <- function(x) {
  x |> spin() |> flip() |> spin()
}

# Spin around an axis
spin <- function(x) {
  vec <- if(is.vector(x)) x else tail(x, 1)
  for (i in 1:3) {
    vec <- c(vec[3], vec[2], -vec[1])
    x <- rbind(x, vec)
  }
  unname(x)
}

# Turn upside down
flip <- function(x) {
  vec <- if(is.vector(x)) x else tail(x, 1)
  vec <- c(-vec[1], -vec[2], vec[3])
  rbind(x, vec) |>
    unname()
}

# Take the next axis as up
reorient <- function(x) {
  vec <- if(is.vector(x)) x else tail(x, 1)
  vec <- c(vec[3], vec[1], vec[2])
  rbind(x, vec) |>
    unname()
}

transform_all <- function(scanner_output, transformation) {
  lapply(seq_len(nrow(scanner_output)), function(x)
    transform(scanner_output[x, ], transformation)) |>
    do.call(what = rbind)
}

transform <- function(coordinate, transformation) {
  index <- abs(transformation)
  sign <- vapply(transformation, function(x) if (x > 0) 1 else -1, numeric(1))
  reindex <- c(coordinate[index[1]], coordinate[index[2]], coordinate[index[3]])
  reindex * sign
}
