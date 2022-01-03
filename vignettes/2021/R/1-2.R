# Sum depths in each window
index <- 1
summed <- c()
while((index + 2) <= length(dat)) {
  summed <- c(summed, sum(dat[index:(index + 2)]))
  index <- index + 1
}

# Count the number of times the depth increases from the previous measurement
sum(diff(summed) > 0)
