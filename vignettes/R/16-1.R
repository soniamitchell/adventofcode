# Read in data ------------------------------------------------------------

dat <- here("inst", "data", "2021", "day16.txt") |>
  scan(what = "character") |>
  strsplit("")

# Functions ---------------------------------------------------------------

hex2bin <- function(hex) {
  #' Convert vector of hexadecimal numbers to 4-bit binary

  strsplit(hex, "")[[1]] |>
    strtoi(base = 16L) |>
    R.utils::intToBin() |>
    paste(collapse = "")
}

bin2dec <- function(bin) {
  #' Convert string of 4-bit binary numbers to decimal

  binary <- stringr::str_pad(bin, 4, pad = "0")
  strtoi(binary, base = 2L) |>
    as.numeric()
}

bin2hex <- function(bin) {
  #' Convert single 4-bit binary number to hexadecimal

  decimal <- bin2dec(bin)
  as.hexmode(decimal) |> toupper()
}

literal_value <- function(string) {
  #' Calculate literal value

  # Split string into 5-bit chunks
  start <- seq(1, nchar(string), 5)
  end <- start + 4
  groups <- substring(string, start, end)

  # Remove empty groups
  if (any(groups == ""))
    groups <- groups[-which(groups == "")]

  # Extract last 4 bits of each group, paste together, and convert to decimal
  bin <- c()
  remaining <-""
  for (x in seq_along(groups)) {
    tmp <- strsplit(gsub("^([[:alnum:]]{1})([[:alnum:]]{4})$",
                         "\\1 \\2", groups[x]), " ")[[1]]
    if (tmp[1] == "0") {
      bin <- c(bin, tmp[2])
      if (x != length(groups)) remaining <- groups[(x + 1):length(groups)]
      break

    } else {
      bin <- c(bin, tmp[2])
    }
  }
  list(literal_value = bin2dec(paste(bin, collapse = "")),
       remaining = paste(remaining, collapse = ""))
}

check_lengthtypeID <- function(string, record) {
  #' Check lengthtypeID

  lengthtypeID <- substr(string, 1, 1)

  if (lengthtypeID == "0") {
    # Number of bits in subpackets
    n_bits <- substr(string, 2, 16) |> strtoi(base = 2L)
    packet <- substring(string, 17)

    # Process subpackets
    remaining <- substr(packet, 1, n_bits)
    parent_remaining <- substring(packet,n_bits + 1)

    while (nchar(remaining) >= 11) { # type = 3, ver = 3, length = 1, hex = 4
      tmp <- check_typeID(remaining, record)
      remaining <- tmp$remaining
      record <- tmp$record
    }

    return(check_typeID(parent_remaining, record))

  } else {
    # Number of subpackets
    n_packets <- substr(string, 2, 12) |> strtoi(base = 2L)
    remaining <- substring(string, 13)

    if (remaining == "") {
      print(remaining)
      return(list(remaining = remaining,
                  record = record))
    }

    # Process subpackets
    for (i in seq_len(n_packets)) {
      tmp <- check_typeID(remaining, record)
      remaining <- tmp$remaining
      record <- tmp$record
    }

    return(list(remaining = remaining, # keep
                record = record))
  }
}

check_typeID <- function(string, record, n = 0) {
  #' Check typeID

  typeID <- substr(string, 4, 6) |> bin2dec()
  version <- substr(string, 1, 3) |> bin2dec()
  remaining <- substring(string, 7)

  if (missing(record)) {
    record <- data.frame(typeID = typeID,
                         lengthtypeID = NA,
                         version = version)
  } else {
    record <- rbind(record, data.frame(typeID = typeID,
                                       lengthtypeID = NA,
                                       version = version))
  }

  print(record)
  print(remaining)
  print(typeID)

  if (typeID == 4) {
    # Literal value
    if (nchar(remaining) != 5) {
      # Pad with zeroes until length is multiple of 4 bits
      min <- ceiling(nchar(remaining) / 4) * 4
      remaining <- stringr::str_pad(remaining, min, pad = "0")
    }

    tmp <- literal_value(remaining)
    value <- tmp$literal_value
    remaining <- tmp$remaining

    return(list(remaining = remaining, # keep
                record = record))

  } else {
    # Operator
    return(check_lengthtypeID(remaining, record))
  }
}

# Run simulation ----------------------------------------------------------

# Decode the structure of your hexadecimal-encoded BITS transmission
string <- hex2bin("38006F45291200")
check_typeID(string)

# What do you get if you add up the version numbers in all packets?
sum(results$version)
