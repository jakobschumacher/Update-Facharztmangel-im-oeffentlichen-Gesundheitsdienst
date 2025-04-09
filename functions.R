read_gbe_files <- function(file, variables) {
  file_path <- paste0("data/", file)

  # Find data boundaries using efficient pattern matching
  header_lines <- readLines(file_path, encoding = "UTF-8")
  data_start <- which.max(grepl("2018", header_lines)) - 1L
  data_end <- which(grepl("\\*\\*\\*", header_lines))[2] - data_start - 3L

  # Read data with precise column specifications
  data <- suppressWarnings(
    readr::read_csv2(
      file = file_path,
      skip = data_start,
      n_max = data_end,
      locale = locale(encoding = "ISO-8859-1"),
      show_col_types = FALSE
    ) %>%
      select(-matches("^X\\d{2}$")) %>%  # Remove auto-generated columns
      discard(~mean(is.na(.x)) == 1) %>%  # Remove empty columns
      tidyr::fill(starts_with("..."), .direction = "down")
  )

  # Set column names using defensive programming
  if(length(variables) > ncol(data)) {
    stop("More variables specified than columns available")
  }
  colnames(data) <- c(variables, tail(names(data), -length(variables)))

  # Efficient data transformation pipeline
  data %>%
    pivot_longer(
      cols = -all_of(variables),
      names_to = "Jahr",
      values_to = "n"
    ) %>%
    mutate(
      across(
        .cols = -c(n, Jahr),
        .fns = ~ str_replace_all(.x, c(
          "Berlin, bis 1990 nur Berlin-West" = "Berlin",
          "Gebiets-/Facharztbezeichnungen insgesamt (incl. ohne Gebiet)" = "Gesamt",
          "Unter 35 Jahre" = "34 Jahre und jünger"
        )) %>%
          as.factor()
      ),
      Jahr = ymd(paste0(Jahr, "-12-31")),
      n = parse_number(
        n,
        locale = locale(decimal_mark = ",", grouping_mark = "."),
        na = c("", "-", "NA")
      ) %>%
        replace_na(0)
    )
}
