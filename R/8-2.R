#' @rdname day8
#' @param dat dat
#' @export
#'
decode_segments <- function(dat) {
  # Determine all of the wire/segment connections
  results <- c()
  for (i in seq_along(dat)) {
    segments <- strsplit(dat[i], " \\|")[[1]][1]
    segments <- strsplit(segments, " ")[[1]]

    # Known
    one <- find_digit(segments, 2)
    four <- find_digit(segments, 4)
    seven <- find_digit(segments, 3)
    eight <- find_digit(segments, 7)

    # Remaining segments
    remaining <- segments[-which(nchar(segments) %in% c(2, 4, 3, 7))]
    # six, nine, zero (6)
    length_six <- remaining[which(nchar(remaining) == 6)]
    six <- contains_subset(length_six, one, FALSE)
    nine <- contains_subset(length_six, four, TRUE)
    zero <- length_six[!(length_six %in% c(six, nine))]
    # two, three, five (5)
    length_five <- remaining[which(nchar(remaining) == 5)]
    five <- subset_of(length_five, six)
    length_five <- length_five[!length_five %in% five]
    three <- subset_of(length_five, nine)
    two <- length_five[!length_five %in% three]

    # Initialise dictionary
    dict <- data.frame(number = 0:9,
                       segments = c(zero, one, two, three, four,
                                    five, six, seven, eight, nine))

    # Decode the four-digit output values
    digits <- strsplit(dat[i], "\\| ")[[1]][2]
    digits <- strsplit(digits, " ")[[1]]
    results[i] <- decode(digits, dict)
  }
  sum(results)
}

#' Find string in `string_vector` of length `num_characters`
#'
find_digit <- function(string_vector, num_characters) {
  string_vector[which(nchar(string_vector) == num_characters)]
}

#' Split string into vector of letters
#'
deconstruct <- function(string) {
  strsplit(string, "")[[1]]
}

#' contains_subset
#'
#' Find (1) which string in `unknown_vector` contains all segments
#' (characters) in `known_string` or (2) which string is the only one that
#' doesn't
#' @param unknown_vector an unknown vector
#' @param known_string a known string
#' @param contains (optional) default is `TRUE`
#'
contains_subset <- function(unknown_vector, known_string, contains = TRUE) {
  results <- c()
  for (i in seq_along(unknown_vector)) {
    known_segments <- deconstruct(known_string)
    unknown_segments <- deconstruct(unknown_vector[i])
    if (contains) {
      results[i] <- all(known_segments %in% unknown_segments)
    } else {
      results[i] <- !all(known_segments %in% unknown_segments)
    }
  }
  unknown_vector[which(results)]
}

#' Find the string in `unknown_vector` whose segments (characters) are a
#' subset of `known_string`
#'
subset_of <- function(unknown_vector, known_string) {
  results <- c()
  for (i in seq_along(unknown_vector)) {
    known_segments <- deconstruct(known_string)
    unknown_segments <- deconstruct(unknown_vector[i])
    results[i] <- all(unknown_segments %in% known_segments)
  }
  unknown_vector[which(results)]
}

#' Decode each string (digit) in `unknown_vector`
#'
decode <- function(unknown_vector, dictionary) {
  code <- c()
  for (i in seq_along(unknown_vector)) {
    # Check against each dictionary entry
    entry <- c()
    for (j in seq_len(nrow(dictionary))) {
      length_match <- nchar(dictionary$segments[j]) == nchar(unknown_vector[i])
      characters_match <- all(deconstruct(dictionary$segments[j]) %in%
                                deconstruct(unknown_vector[i]))
      entry[j] <- length_match && characters_match
    }
    code[i] <- dictionary$number[which(entry)]
  }
  as.numeric(paste(code, collapse = ""))
}
