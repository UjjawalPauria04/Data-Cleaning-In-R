#   DATA CLEANING PROJECT
# ---- 1. Create the messy raw data ----------------------------
raw <- data.frame(
  ID        = c(1, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10),
  Name      = c("  alice johnson", "BOB SMITH", "Carol White", "Carol White",
                "david brown ", "Eve Davis", "frank miller", "Grace Lee",
                "", "Henry Ford", "Ivy Chen"),
  Age       = c("28", "34", "N/A", "N/A", "45", "-5", "200", "31", "27", "", "39"),
  City      = c("new york", "NEW YORK", "Los Angeles", "Los Angeles", "chicago ",
                "Chicago", "LA", "los angeles", "Houston", "houston", "Houston"),
  Salary    = c("$65,000", "72000", "$58,500", "$58,500", "81,000", "NA",
                "95000", "$70,250", "62000", "$55,000", ""),
  Join_Date = c("2021-03-15", "15/04/2020", "2019-07-01", "2019-07-01",
                "2022-01-10", "2020-11-30", "03/02/2021", "2018-09-12",
                "2023-05-05", "2021-08-21", "2022-12-01"),
  Email     = c("alice@mail.com", "bob@mail", "carol@mail.com", "carol@mail.com",
                "DAVID@MAIL.COM", "eve@mail.com", "frank.mail.com",
                "grace@mail.com", NA, "henry@mail.com", "ivy@mail.com"),
  stringsAsFactors = FALSE
)

dir.create("cleaning_output", showWarnings = FALSE)
write.csv(raw, "cleaning_output/raw_data.csv", row.names = FALSE)

cat("\n=== 2. RAW DATA INSPECTION ===\n")
print(raw)
cat("\n--- Structure ---\n")
str(raw)

df <- raw   

names(df) <- tolower(names(df))      


char_cols <- names(df)[sapply(df, is.character)]
for (col in char_cols) {
  df[[col]] <- trimws(df[[col]])
  df[[col]][df[[col]] %in% c("", "NA", "N/A", "null", "NULL")] <- NA
}

df$name <- tools::toTitleCase(tolower(df$name))
df$name[is.na(df$name)] <- "Unknown"


df$city <- tolower(df$city)
df$city[df$city == "la"] <- "los angeles"
df$city <- tools::toTitleCase(df$city)


df$email <- tolower(df$email)
valid_email <- grepl("^[[:alnum:]._-]+@[[:alnum:].-]+\\.[a-z]{2,}$", df$email)
n_bad_email <- sum(!valid_email & !is.na(df$email))
df$email[!valid_email] <- NA


df$age <- suppressWarnings(as.numeric(df$age))
n_bad_age <- sum(df$age < 0 | df$age > 100, na.rm = TRUE)
df$age[df$age < 0 | df$age > 100] <- NA


df$salary <- suppressWarnings(as.numeric(gsub("[$,]", "", df$salary)))


d_iso <- as.Date(df$join_date, format = "%Y-%m-%d")   # 2021-03-15
d_dmy <- as.Date(df$join_date, format = "%d/%m/%Y")   # 15/04/2020
df$join_date <- d_iso
df$join_date[is.na(df$join_date)] <- d_dmy[is.na(df$join_date)]

cat("\n=== 8. MISSING VALUES BEFORE IMPUTATION ===\n")
print(colSums(is.na(df)))

df$age[is.na(df$age)]       <- median(df$age, na.rm = TRUE)
df$salary[is.na(df$salary)] <- median(df$salary, na.rm = TRUE)

n_before <- nrow(df)
df <- df[!duplicated(df), ]
n_dupes  <- n_before - nrow(df)
rownames(df) <- NULL

cat("\n=== 10. VALIDATION ===\n")
stopifnot(!anyDuplicated(df$id))                 # ids are unique
stopifnot(all(df$age >= 0 & df$age <= 100))     # ages are sensible
stopifnot(is.numeric(df$salary), inherits(df$join_date, "Date"))
cat("All checks passed.\n")

cat("\n=== 11. CLEANED DATA ===\n")
print(df)
cat("\n--- Structure ---\n")
str(df)

write.csv(df, "cleaning_output/clean_data.csv", row.names = FALSE)

cat("\n=== CLEANING REPORT ===\n")
cat("Rows before            :", nrow(raw), "\n")
cat("Rows after             :", nrow(df), "\n")
cat("Duplicate rows removed :", n_dupes, "\n")
cat("Impossible ages fixed  :", n_bad_age, "\n")
cat("Invalid emails removed :", n_bad_email, "\n")
cat("Distinct cities        :", paste(sort(unique(df$city)), collapse = ", "), "\n")
cat("\nSaved to cleaning_output/raw_data.csv and clean_data.csv\n")