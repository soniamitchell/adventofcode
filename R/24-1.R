#' Day 24: Arithmetic Logic Unit
#' @source <https://adventofcode.com/2021/day/24>
#' @name day24
#'
NULL

#' @rdname day24
#' @param path file path
#' @export
#'
read_day24 <- function(path) {
  path |>
    readLines()


  # |>
  #   strsplit(" ") |>
  #   lapply(\(x) if (length(x) == 2) c(x, NA) else x) |>
  #   do.call(what = rbind) |>
  #   data.frame() |>
  #   setNames(c("instruction", "x1", "x2"))
}

arithmetic_logic_unit <- R6::R6Class("ALU", list(
  w = NULL,
  x = NULL,
  y = NULL,
  z = NULL,
  model_number = NULL,

  initialize = function(model_number) {
    self$w <- 0
    self$x <- 0
    self$y <- 0
    self$z <- 0
    self$model_number <- as.numeric(strsplit(model_number, "")[[1]])
    invisible(self)
  },

  print = function(...) {
    cat("w =", self$w)
    cat("\nx =", self$x)
    cat("\ny =", self$y)
    cat("\nz =", self$z)
    cat("\nmodel number =", self$model_number)
    invisible(self)
  },

  inp = function(var) {
    self[[var]] <- self$model_number[1]
    self$model_number <- tail(self$model_number, -1)
    invisible(self)
  },

  add = function(x1, x2) {
    tmp <- ifelse(grepl("\\d", x2), as.numeric(x2), self[[x2]])
    self[[x1]] <- self[[x1]] + tmp
    invisible(self)
  },

  mul = function(x1, x2) {
    tmp <- ifelse(grepl("\\d", x2), as.numeric(x2), self[[x2]])
    self[[x1]] <- self[[x1]] * tmp
    invisible(self)
  },

  div = function(x1, x2) {
    tmp <- ifelse(grepl("\\d", x2), as.numeric(x2), self[[x2]])
    if (tmp != 0) {
      self[[x1]] <- ifelse(tmp > 0,
                           floor(self[[x1]] / tmp),
                           ceiling(self[[x1]] / tmp))
    }

    invisible(self)
  },

  mod = function(x1, x2) {
    tmp <- ifelse(grepl("\\d", x2), as.numeric(x2), self[[x2]])
    if (!(self[[x1]] < 0 | tmp <= 0))
      self[[x1]] <- self[[x1]] %% tmp
    invisible(self)
  },

  eql = function(x1, x2) {
    tmp <- ifelse(grepl("\\d", x2), as.numeric(x2), self[[x2]])
    self[[x1]] <- ifelse(self[[x1]] == tmp, 1, 0)
    invisible(self)
  }
))

calculate <- function(alu, instruction, x) {
  switch(instruction,
         inp = alu$inp(x[1]),
         add = alu$add(x[1], x[2]),
         mul = alu$mul(x[1], x[2]),
         div = alu$div(x[1], x[2]),
         mod = alu$mod(x[1], x[2]),
         eql = alu$eql(x[1], x[2]))
}
