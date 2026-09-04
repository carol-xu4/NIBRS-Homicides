#-------------------------------------------------------------------------
# clean_nibrs_homicide.R
#
# Purpose:
#   Read the three raw ICPSR 39270 (NIBRS 2023 Extract Files) tables --
#   Incident-Level (DS0003), Victim-Level (DS0004), Offender-Level (DS0006)
#   -- and reduce each to only the rows related to criminal homicide
#   (UCR offense codes 91 = Murder/Nonnegligent Manslaughter and
#   92 = Negligent Manslaughter). Justifiable Homicide (93) is EXCLUDED
#   by design, since it is not a crime.
#
#   Cleaned files are written to data/output/ as both .rds (fast to
#   reload in R, preserves factor labels) and .csv (for use outside R).
#
# Inputs (edit INPUT_DIR below if your folder layout differs):
#   data/input/ICPSR_39270/DS0003/39270-0003-Data.rda   (incident-level)
#   data/input/ICPSR_39270/DS0004/39270-0004-Data.rda   (victim-level)
#   data/input/ICPSR_39270/DS0006/39270-0006-Data.rda   (offender-level)
#
# Outputs:
#   data/output/incidents_homicide.rds / .csv
#   data/output/victims_homicide.rds   / .csv
#   data/output/offenders_homicide.rds / .csv
#   data/output/cleaning_summary.csv   (row counts before/after, for a
#                                        quick sanity check)
#
# IMPORTANT CAVEATS baked into this script (read before trusting output):
#
#  1. NIBRS offense codes arrive from ICPSR as FACTOR variables whose
#     levels look like "(091) Murder/Nonnegligent Manslaughter" -- the
#     numeric code is embedded as a zero-padded prefix in the label, not
#     stored as a plain number. is_homicide_code() below matches on that
#     prefix so it works whether the column comes in as a factor,
#     character, or plain numeric (defensive, since I could not execute
#     this script against your actual files to confirm the exact class
#     R assigns on your machine -- please check the console messages
#     the first time you run it).
#
#  2. The Offender Segment in NIBRS is NOT offense-specific -- an
#     offender record is linked to the whole INCIDENT, not to one
#     particular offense within it. So "homicide offenders" here means
#     "offenders present in an incident that included a homicide
#     offense," which is the most precise level NIBRS supports. An
#     incident with, e.g., robbery + murder will show the same offender
#     set for both offenses.
#
#  3. The Victim Segment DOES carry its own offense-code list per
#     victim (V4007-V4016), so victims are filtered more precisely: a
#     victim only counts as a "homicide victim" if 91/92 appears among
#     THEIR OWN linked offense codes (not merely because they appear
#     somewhere in an incident that happens to include a homicide).
#     We also restrict to Type of Victim = "Individual" (V4017==1),
#     since only individual persons carry age/sex/race/ethnicity.
#
#  4. Unidentified ("unsolved") offenders are NOT dropped. NIBRS still
#     writes an Offender Segment for them, with age/sex/race/ethnicity
#     coded as -7 "Unknown/missing/DNR" (or 0 for age specifically,
#     which NIBRS uses as its own "unknown age" convention). Do not
#     filter these out -- excluding them would bias offender demographics
#     toward solved cases only.
#-------------------------------------------------------------------------

suppressMessages({
  library(dplyr)
})

## ---- 0. Paths -----------------------------------------------------------

INPUT_DIR  <- "data/input/ICPSR_39270"
OUTPUT_DIR <- "data/output"

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

incident_rda  <- file.path(INPUT_DIR, "DS0003", "39270-0003-Data.rda")
victim_rda    <- file.path(INPUT_DIR, "DS0004", "39270-0004-Data.rda")
offender_rda  <- file.path(INPUT_DIR, "DS0006", "39270-0006-Data.rda")

stopifnot(file.exists(incident_rda), file.exists(victim_rda), file.exists(offender_rda))

## ---- 1. Helpers -----------------------------------------------------------

# Load an ICPSR .rda file and return its one data frame, regardless of
# what R happened to name the object inside the file (ICPSR typically
# uses names like da39270.0003, but this does not rely on that).
load_icpsr_df <- function(path) {
  env <- new.env()
  loaded_names <- load(path, envir = env)
  obj_name <- loaded_names[vapply(loaded_names, function(n) is.data.frame(get(n, envir = env)), logical(1))][1]
  if (is.na(obj_name)) stop("No data frame found inside ", path)
  message("  loaded object '", obj_name, "' (", nrow(get(obj_name, envir = env)), " rows) from ", path)
  get(obj_name, envir = env)
}

# TRUE where x represents UCR offense code 91 (Murder/Nonnegligent
# Manslaughter) or 92 (Negligent Manslaughter). Handles factor labels
# like "(091) Murder/Nonnegligent Manslaughter", plain character "91",
# or numeric 91 -- whatever class the column ends up being.
is_homicide_code <- function(x) {
  x_chr <- as.character(x)
  # matches a leading "(091)"/"(092)" style ICPSR factor label ...
  by_label <- grepl("^\\(0*9[12]\\)", x_chr)
  # ... or a bare numeric code of 91/92 (with or without leading zeros)
  by_number <- suppressWarnings(as.numeric(x_chr)) %in% c(91, 92)
  by_label | by_number
}

# Build a single incident-key column so the three files can be matched
# to each other (NIBRS incidents are uniquely identified by agency ORI
# + incident number, not by INCNUM alone).
add_incident_key <- function(df) {
  df %>% mutate(incident_key = paste(ORI, INCNUM, sep = "_"))
}

n_rows <- function(df) format(nrow(df), big.mark = ",")

summary_rows <- list()

## ---- 2. Incident-level file (DS0003): identify homicide incidents -------

message("Reading incident-level file (DS0003) ...")
incidents <- load_icpsr_df(incident_rda)
incidents <- add_incident_key(incidents)

n_incidents_total <- nrow(incidents)

# An incident counts as a "homicide incident" if ANY of its up to 3
# listed offense codes is 91 or 92.
offense_cols <- intersect(c("V20061", "V20062", "V20063"), names(incidents))
stopifnot(length(offense_cols) > 0)

incidents$is_homicide_incident <- Reduce(`|`, lapply(offense_cols, function(cn) is_homicide_code(incidents[[cn]])))

incidents_homicide <- incidents %>% filter(is_homicide_incident)
homicide_incident_keys <- unique(incidents_homicide$incident_key)

message("  homicide incidents: ", n_rows(incidents_homicide), " of ", n_rows(incidents), " total incidents")

summary_rows[["incidents"]] <- data.frame(
  file = "incidents (DS0003)",
  rows_before = n_incidents_total,
  rows_after = nrow(incidents_homicide)
)

saveRDS(incidents_homicide, file.path(OUTPUT_DIR, "incidents_homicide.rds"))
write.csv(incidents_homicide, file.path(OUTPUT_DIR, "incidents_homicide.csv"), row.names = FALSE, na = "")

rm(incidents); gc()

## ---- 3. Victim-level file (DS0004): keep individual homicide victims ----

message("Reading victim-level file (DS0004) ...")
victims <- load_icpsr_df(victim_rda)
victims <- add_incident_key(victims)

n_victims_total <- nrow(victims)

# A victim's own linked offense codes (up to 10 slots).
victim_offense_cols <- intersect(paste0("V40", sprintf("%02d", 7:16)), names(victims))
if (length(victim_offense_cols) == 0) {
  # fallback naming, in case ICPSR ships these without the leading zero
  victim_offense_cols <- intersect(paste0("V40", 7:16), names(victims))
}
stopifnot(length(victim_offense_cols) > 0)

victims$is_homicide_victim_offense <- Reduce(`|`, lapply(victim_offense_cols, function(cn) is_homicide_code(victims[[cn]])))

# Type of victim: 1 = Individual (see V4017). Non-individual victim
# types (business, society, government, etc.) never carry age/sex/race,
# so they are dropped for a victim demographics file, but note they can
# still appear as a "victim" of certain non-homicide offenses in the
# same incident -- irrelevant here since we're already filtering to the
# homicide offense itself.
is_individual <- as.character(victims$V4017) %in% c("1", "(1) Individual") |
  grepl("^\\(0*1\\)", as.character(victims$V4017))

victims_homicide <- victims %>%
  filter(is_homicide_victim_offense, is_individual) %>%
  filter(incident_key %in% homicide_incident_keys)  # cross-check against DS0003; should already all be TRUE

message("  homicide victims (individual, own-offense = homicide): ", n_rows(victims_homicide), " of ", n_rows(victims), " total victim records")

summary_rows[["victims"]] <- data.frame(
  file = "victims (DS0004)",
  rows_before = n_victims_total,
  rows_after = nrow(victims_homicide)
)

saveRDS(victims_homicide, file.path(OUTPUT_DIR, "victims_homicide.rds"))
write.csv(victims_homicide, file.path(OUTPUT_DIR, "victims_homicide.csv"), row.names = FALSE, na = "")

rm(victims); gc()

## ---- 4. Offender-level file (DS0006): keep offenders in homicide incidents

message("Reading offender-level file (DS0006) ...")
offenders <- load_icpsr_df(offender_rda)
offenders <- add_incident_key(offenders)

n_offenders_total <- nrow(offenders)

offenders_homicide <- offenders %>% filter(incident_key %in% homicide_incident_keys)

message("  offenders in homicide incidents: ", n_rows(offenders_homicide), " of ", n_rows(offenders), " total offender records")
message("  (recall: NIBRS offender records are incident-linked, not offense-specific -- see script header)")

summary_rows[["offenders"]] <- data.frame(
  file = "offenders (DS0006)",
  rows_before = n_offenders_total,
  rows_after = nrow(offenders_homicide)
)

saveRDS(offenders_homicide, file.path(OUTPUT_DIR, "offenders_homicide.rds"))
write.csv(offenders_homicide, file.path(OUTPUT_DIR, "offenders_homicide.csv"), row.names = FALSE, na = "")

rm(offenders); gc()

## ---- 5. Write a cleaning summary for a quick sanity check ----------------

summary_df <- do.call(rbind, summary_rows)
write.csv(summary_df, file.path(OUTPUT_DIR, "cleaning_summary.csv"), row.names = FALSE)

message("\nDone. Cleaned files written to ", OUTPUT_DIR, "/")
print(summary_df, row.names = FALSE)
