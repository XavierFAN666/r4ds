library(tidyverse)

ggplot(diamonds,aes(carat,price)) +
  geom_hex()
ggsave("diamonds.pdf")

write_csv(diamonds,"diamonds.csv")

# load data ----------------------------------------------------

show_summary <- function(df,fun) {
  num_df <- vector("list")
  for (i in seq_along(df)) {
    if (is.numeric(df[[i]])) {
      num_df[[i]] <- df[[i]]
      names(num_df)[i] <- names(df)[i]
    }
    num_df <- as.data.frame(num_df)
  }
  num_df
  out <- vector("double", length(num_df))
  result <- vector("character")
  for (i in seq_along(num_df)) {
    out[i] <- fun(num_df[[i]])
  }
  for (i in seq_along(out)) {
    result[i] <- stringr::str_c(names(num_df)[i], ": ", out[i])
  }
  result
}

# 精简版show_summary
show_sum <- function(df, f, ...) {
  out <- map(keep(df, is.numeric), f, ...)
  result <- vector("character")
  for (i in seq_along(out)) {
    result[i] <- stringr::str_c(names(out)[i], ": ", out[i])
  }
  result
}

show_mean <- function(df, digits = 2) {
  # Get max length of all variable names in the dataset
  maxstr <- max(str_length(names(df)))
  for (nm in names(df)) {
    if (is.numeric(df[[nm]])) {
      cat(
        str_c(str_pad(str_c(nm, ":"), maxstr + 1L, side = "right"),
              format(mean(df[[nm]]), digits = digits, nsmall = digits),
              sep = " "
        ),
        "\n"
      )
    }
  }
}
