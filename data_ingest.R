# ============================================================
# Google Sheets Ingest Script
# Source: NCAA / Athletic Stats Workbook (2022–2025)
# ============================================================

library(googlesheets4)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)
library(stringdist)

# ------------------------------------------------------------
# 1. Authenticate
# ------------------------------------------------------------
gs4_auth()

# ------------------------------------------------------------
# 2. Configuration
# ------------------------------------------------------------
sheet_url <- "https://docs.google.com/spreadsheets/d/1YTd-V3mQgiiYExXjssISjLQru0PqwYzQACZiDRRi884"

years   <- 2022:2025
seasons <- c("Spring", "Fall")

tab_names <- expand.grid(season = seasons, year = years) |>
  arrange(year, season) |>
  mutate(tab = paste(season, year)) |>
  pull(tab)

# ------------------------------------------------------------
# 3. Read Glossary (column header reference)
# ------------------------------------------------------------
glossary <- read_sheet(sheet_url, sheet = "Glossary") |>
  select(col_head = col_head, stat_name = `Stat Name`) |>
  filter(!is.na(col_head))

cat("Glossary loaded:", nrow(glossary), "column definitions\n")

# ------------------------------------------------------------
# 4. Safe reader — forces all columns to character on read
#    to prevent type-mismatch errors in list_rbind()
# ------------------------------------------------------------
safe_read <- possibly(read_sheet, otherwise = NULL)

read_tab <- function(tab) {
  message("Reading: ", tab)
  
  result <- safe_read(
    sheet_url, 
    sheet = tab,
    col_types = "c"   # <-- read everything as character
  )
  
  if (is.null(result)) {
    warning("  !! Failed to read tab: ", tab)
    return(NULL)
  }
  
  if (nrow(result) == 0) {
    warning("  !! Tab is empty: ", tab)
    return(NULL)
  }
  
  # Drop duplicate columns (keep first occurrence of each name)
  # This handles cases where googlesheets4 appends ...N to dupes
  base_names <- sub("\\.\\.\\.\\d+$", "", names(result))  # strip ...N suffix
  result <- result[, !duplicated(base_names)]              # keep first of each
  names(result) <- base_names[!duplicated(base_names)]     # restore clean names
  
  result |>
    mutate(
      source_tab = tab,
      season     = str_extract(tab, "^\\w+"),
      year       = as.integer(str_extract(tab, "\\d{4}")),
      .before    = 1
    )
}


# ------------------------------------------------------------
# 5. Read, stack, then re-cast types
# ------------------------------------------------------------
library(readr)

combined_df <- tab_names |>
  map(read_tab) |>
  compact() |>
  list_rbind() |>
  type_convert() |>   # re-infers numeric, date, logical, etc.
  filter(Number != "Totals")

cat("\nIngestion complete!\n")
cat("  Tabs successfully read:", length(unique(combined_df$source_tab)), "\n")
cat("  Total rows:           ", nrow(combined_df), "\n")
cat("  Total columns:        ", ncol(combined_df), "\n")

# ------------------------------------------------------------
# 6. Attach full stat names from glossary as a named vector
#    (useful for plot labels, table headers, etc.)
# ------------------------------------------------------------
col_label <- glossary |>
  filter(col_head %in% names(combined_df)) |>
  deframe()   # creates named vector: col_label["pts"] -> "Points"

# Example use:
#   col_label["pts"]          # returns full stat name
#   col_label[names(data)]    # renames columns in bulk

# ------------------------------------------------------------
# 7. Quick diagnostics — see which columns appear in which tabs
# ------------------------------------------------------------
coverage <- combined_df |>
  group_by(source_tab) |>
  summarise(
    rows         = n(),
    cols_present = sum(sapply(across(everything()), \(x) !all(is.na(x)))),
    .groups      = "drop"
  ) 

print(coverage)

# Which columns are missing (all-NA) per tab
missing_by_tab <- combined_df |>
  group_by(source_tab) |>
  summarise(
    across(everything(), \(x) all(is.na(x))),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(-source_tab, names_to = "col", values_to = "all_na") |>
  filter(all_na, !col %in% c("season", "year")) |>
  group_by(source_tab) |>
  summarise(missing_cols = paste(col, collapse = ", "), .groups = "drop")

if (nrow(missing_by_tab) > 0) {
  cat("\nColumns absent (all-NA) per tab:\n")
  print(missing_by_tab)
} else {
  cat("\nAll columns present across all tabs.\n")
}

# ------------------------------------------------------------
# Duplicate Detection via Fuzzy Name Matching
# Blocked by first initial (cross-season, cross-year)
# ------------------------------------------------------------

# 1. Build a clean name field to match on
name_df <- combined_df |>
  mutate(
    row_id      = row_number(),
    first_clean = str_squish(str_to_upper(First)),
    last_clean  = str_squish(str_to_upper(Last)),
    full_name   = case_when(
      is.na(last_clean) | last_clean == "" ~ first_clean,
      TRUE ~ paste(first_clean, last_clean)
    ),
    first_initial = str_sub(first_clean, 1, 1)
  ) |>
  select(row_id, source_tab, season, year, First, Last, full_name, first_initial)

# 2. Fuzzy match within each first-initial block
fuzzy_dupes <- name_df |>
  group_by(first_initial) |>
  group_modify(\(group, keys) {
    n <- nrow(group)
    if (n < 2) return(tibble())
    
    pairs <- combn(n, 2, simplify = FALSE)
    
    map_dfr(pairs, \(idx) {
      i <- idx[1]; j <- idx[2]
      
      name_i <- group$full_name[i]
      name_j <- group$full_name[j]
      
      if (is.na(name_i) | is.na(name_j)) return(tibble())
      
      dist <- stringdist(name_i, name_j, method = "jw")
      
      if (dist > 0 & dist < 0.15) {
        tibble(
          row_id_1      = group$row_id[i],
          source_tab_1  = group$source_tab[i],
          season_1      = group$season[i],
          year_1        = group$year[i],
          name_1        = group$full_name[i],
          first_1       = group$First[i],
          last_1        = group$Last[i],
          row_id_2      = group$row_id[j],
          source_tab_2  = group$source_tab[j],
          season_2      = group$season[j],
          year_2        = group$year[j],
          name_2        = group$full_name[j],
          first_2       = group$First[j],
          last_2        = group$Last[j],
          jw_distance   = round(dist, 4),
          flag          = case_when(
            is.na(group$Last[i]) | is.na(group$Last[j]) ~ "missing last name",
            str_to_upper(group$First[i]) == str_to_upper(group$First[j]) ~ "same first, diff last",
            str_to_upper(group$Last[i])  == str_to_upper(group$Last[j])  ~ "same last, diff first",
            TRUE ~ "similar spelling"
          )
        )
      } else {
        tibble()
      }
    })
  }) |>
  ungroup() |>
  arrange(jw_distance, first_initial)   # first_initial is restored by group_modify automatically

# 3. Review results
cat("Possible duplicate pairs found:", nrow(fuzzy_dupes), "\n\n")
print(fuzzy_dupes)


