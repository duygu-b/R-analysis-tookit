#  -------------------- AMBIGUOUS SENTENCE ANALYSIS --------------------
# Ambiguous sentences, part 1: picking out the trials

# install.packages("readr")
library(readr)
library(dplyr)

# One participant's pre-processed file
file <- "/documents/xxxx_pre_processed.csv"

out_dir   <- dirname(file)                               # folder the file sits in
base_name <- tools::file_path_sans_ext(basename(file))   # file name without the .csv

data <- read_csv(file)

# A free-sort trial only counts if the participant got it right. The answer is recorded two rows above the free-sort row, so lag(correct, 2) reaches it.
# Trials with a wrong answer are dropped here.
data <- data %>%
  filter(!(task == "free-sort" & dplyr::lag(correct, 2) == FALSE))

# Keep the ambiguous sentences only
amb <- data %>%
  filter(condition == "ambiguous")

# Drop columns 6-10 and 12-14, which are not needed for the gaze analysis
amb <- amb[, -c(6:10, 12:14)]

# Column 5 is still column 5, because everything removed above came after it.
# Its values are read in as text, so they are turned into numbers here.
amb[[5]] <- as.numeric(amb[[5]])

# Sort the rows by sentence number. sentence_id looks like "amb10", so "amb"
# is cut off and what is left ("10") is read as a number.
amb <- amb[order(as.numeric(sub("amb", "", amb$sentence_id))), ]

# Save as a new file, with _amb added to the name
write_csv(amb, file.path(out_dir, paste0(base_name, "_amb.csv")))

# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Ambiguous sentences, part 2: when each word was spoken

file <- "/documents/xxxx_pre_processed_amb.csv" #using the file created in step 1
out_dir   <- dirname(file)
base_name <- tools::file_path_sans_ext(basename(file))

amb <- read_csv(file)

# The sentence number of each row: "amb10" becomes 10
sent_num <- as.numeric(sub("amb", "", amb$sentence_id))

# Two checks before going any further. Every number has to be between 1 and 20, and no sentence may turn up twice. The script stops if either fails.
if (any(is.na(sent_num)) || any(sent_num < 1 | sent_num > 20)) stop("sentence_id'lerde beklenmeyen değer var!")
if (anyDuplicated(sent_num)) stop("Aynı sentence_id birden fazla kez geçiyor!")


# When the second preposition starts in each sentence, in ms counted from the start of the audio. The list runs amb1 to amb20, so a sentence's number is
# also its place in the list.
onset_values <- c(
  1760, 1835, 1990, 1620, 1910, 2000, 1840, 1830, 1860, 1870,
  1710, 1540, 1870, 1500, 1760, 2020, 2110, 1710, 1660, 1860
)

# The same moment, but on the recording's own clock, so that it can be compared with the gaze timestamps. Looked up by sentence number, not by the
# row's position in the file.
amb$roi_onset <- amb$audio_start_global + onset_values[sent_num]

shift <- 200   # ms, added to every onset below, this is to compansate the saccade delay

amb$roi_onset_shifted <- amb$roi_onset + shift


# When each word starts, again in ms from the start of the audio. Each row is
# one word (W1 to W9), and each column is one sentence (amb1 to amb20).
word_onsets <- rbind(
  c(270, 280, 280, 280, 260, 270, 280, 270, 280, 280, 270, 270, 270, 270, 270, 270, 270, 260, 270, 290),
  c(490, 510, 490, 500, 500, 500, 500, 520, 500, 480, 480, 500, 500, 500, 500, 500, 500, 490, 490, 500),
  c(590, 650, 670, 630, 630, 590, 650, 650, 630, 610, 570, 660, 660, 630, 600, 590, 590, 590, 620, 600),
  c(990, 1060, 1190, 960, 1010, 1160, 1120, 1045, 1180, 1230, 1050, 970, 970, 890, 980, 1200, 1210, 1010, 1000, 1030),
  c(1120, 1190, 1350, 1060, 1130, 1300, 1300, 1190, 1350, 1360, 1210, 1080, 1160, 1030, 1130, 1310, 1420, 1170, 1190, 1290),
  c(1190, 1310, 1440, 1170, 1230, 1390, 1410, 1250, 1440, 1500, 1280, 1180, 1220, 1120, 1190, 1420, 1480, 1270, 1260, 1360),
  c(1760, 1835, 1990, 1620, 1910, 2000, 1840, 1830, 1860, 1870, 1710, 1540, 1870, 1500, 1760, 2020, 2110, 1710, 1660, 1860),
  c(2050, 2120, 2260, 1910, 2210, 2260, 2100, 2180, 2110, 2190, 2010, 1790, 2130, 1750, 2040, 2310, 2380, 1950, 1950, 2140),
  c(2170, 2250, 2380, 2000, 2300, 2350, 2220, 2320, 2240, 2300, 2120, 1920, 2240, 1860, 2120, 2430, 2510, 2030, 2080, 2210)
)


# Turn those into the columns W1_onset ... W9_onset, on the recording's clock
for (w in 1:9) {
  amb[[paste0("W", w, "_onset")]] <- amb$audio_start_global + word_onsets[w, sent_num]
}

# Ambiguous sentences have only nine words, so these two columns stay empty
amb$W10_onset <- NA
amb$W11_onset <- NA

# The window closes 1500 ms after the last word starts
amb$roi_onset_end <- amb$W9_onset + shift + 1500

write_csv(amb, file.path(out_dir, paste0(base_name, "_gaze.csv")))



# Ambiguous sentences, part 3: where the participant looked

library(jsonlite)
library(readr)

file <- "/documents/xxxx_pre_processed_amb_gaze.csv" # using the file created in step 2

amb <- read_csv(file, show_col_types = FALSE)


# Target count: how many gaze samples landed on the target, trial by trial:
target_counts <- c()

for (i in 1:nrow(amb)) {

  locations <- fromJSON(amb$init_locations[i]) #  locations = which quadrant each image was in, read out of the init_locations column

  target <- amb$target_image[i]

  target_location <- locations[basename(locations$src) == target, ]

  target_quadrant <- target_location$quadrant #  target_quadrant = the quadrant of the target, found by its file name

  gaze <- fromJSON(amb$gaze_samples[i]) #  gaze = the gaze recording, read out of the gaze_samples column; every sample has a time t and a quadrant

  #  gaze_after_prep  = only the samples inside the window, from the shifted preposition onset to the end of the window
  gaze_after_prep <- gaze[gaze$t > amb$roi_onset_shifted[i] & gaze$t < amb$roi_onset_end[i], ] 

  count <- sum(gaze_after_prep$quadrant == target_quadrant) #  count = how many of those samples fell in the target's quadrant

  target_counts[i] <- count
}

amb$target_count <- target_counts


# Distractor count: the same count again, this time for the distractor

distractor_counts <- c()

for (i in 1:nrow(amb)) {

  locations <- fromJSON(amb$init_locations[i])

  distractor <- amb$distractor[i]

  distractor_location <- locations[basename(locations$src) == distractor, ]

  distractor_quadrant <- distractor_location$quadrant

  gaze <- fromJSON(amb$gaze_samples[i])

  gaze_after_prep <- gaze[gaze$t > amb$roi_onset_shifted[i] & gaze$t < amb$roi_onset_end[i], ]

  count <- sum(gaze_after_prep$quadrant == distractor_quadrant)

  distractor_counts[i] <- count
}

amb$distractor_count <- distractor_counts


# Target proportion: 

total_gaze_counts <- c()
target_gaze_proportions <- c()

for (i in 1:nrow(amb)) {

  gaze <- fromJSON(amb$gaze_samples[i])

  gaze_after_prep <- gaze[gaze$t > amb$roi_onset_shifted[i] & gaze$t < amb$roi_onset_end[i], ]

  total_count <- nrow(gaze_after_prep) #total_count = every sample inside the window, wherever it landed

  # proportion  = the target count divided by that total. If no samples were recorded at all, the trial is left empty rather than dividing by zero.
  proportion <- if (total_count > 0) amb$target_count[i] / total_count else NA 

  total_gaze_counts[i] <- total_count
  target_gaze_proportions[i] <- proportion
}

amb$total_gaze_count <- total_gaze_counts
amb$target_gaze_proportion <- target_gaze_proportions


# Distractor proportion: the same share, this time for the distractor

distractor_gaze_proportions <- c()

for (i in 1:nrow(amb)) {

  gaze <- fromJSON(amb$gaze_samples[i])

  gaze_after_prep <- gaze[gaze$t > amb$roi_onset_shifted[i] & gaze$t < amb$roi_onset_end[i], ]

  total_count <- nrow(gaze_after_prep)

  proportion <- if (total_count > 0) amb$distractor_count[i] / total_count else NA

  distractor_gaze_proportions[i] <- proportion
}

amb$distractor_gaze_proportion <- distractor_gaze_proportions


# Proportions word by word: he same shares again, but one per word instead of one for the whole window:
# A word with no gaze samples in its window keeps its empty value.

shift <- 200   # ms

for (w in 1:11) {
  amb[[paste0("W", w, "_target_prop")]]     <- NA
  amb[[paste0("W", w, "_distractor_prop")]] <- NA
}

for (i in 1:nrow(amb)) {

  # Which quadrant the target and the distractor were in on this trial
  locations <- fromJSON(amb$init_locations[i])
  target_quadrant     <- locations$quadrant[basename(locations$src) == amb$target_image[i]]
  distractor_quadrant <- locations$quadrant[basename(locations$src) == amb$distractor[i]]

  gaze <- fromJSON(amb$gaze_samples[i])

  for (w in 1:9) {

    start <- amb[[paste0("W", w, "_onset")]][i] + shift # start = that word's onset, plus the 200 ms shift

    if (w < 9) {
      end <- amb[[paste0("W", w + 1, "_onset")]][i] + shift 
    } else {
      end <- start + 1500 # end = the next word's onset, plus the same shift. The last word has no next word, so its window is 1500 ms long.
    }

    # The gaze samples recorded while this word was being spoken
    gaze_word <- gaze[gaze$t >= start & gaze$t < end, ]
    total <- nrow(gaze_word)

    if (total > 0) {
      target_prop     <- sum(gaze_word$quadrant == target_quadrant) / total
      distractor_prop <- sum(gaze_word$quadrant == distractor_quadrant) / total

      amb[[paste0("W", w, "_target_prop")]][i]     <- target_prop
      amb[[paste0("W", w, "_distractor_prop")]][i] <- distractor_prop
    }
  }
}

amb$shift_ms <- shift   # the shift is written into the file as a record


# Save over the same file
write_csv(amb, file)




#  -------------------- UNAMBIGUOUS SENTENCE ANALYSIS --------------------
Gaze proportions unambiguous · R
# Unambiguous sentences, part 1: picking out the trials
 
# install.packages("readr")
library(readr)
library(dplyr)
 
# One participant's pre-processed file
file <- "/documents/xxxx_pre_processed.csv"
 
out_dir   <- dirname(file)                               # folder the file sits in
base_name <- tools::file_path_sans_ext(basename(file))   # file name without the .csv
 
data <- read_csv(file)
 
# A free-sort trial only counts if the participant got it right. The answer is recorded two rows above the free-sort row, so lag(correct, 2) reaches it.
# Trials with a wrong answer are dropped here.
data <- data %>%
  filter(!(task == "free-sort" & dplyr::lag(correct, 2) == FALSE))
 
# Keep the unambiguous sentences only
unamb <- data %>%
  filter(condition == "unambiguous")
 
# Drop columns 6-10 and 12-14, which are not needed for the gaze analysis
unamb <- unamb[, -c(6:10, 12:14)]
 
# Column 5 is still column 5, because everything removed above came after it.
# Its values are read in as text, so they are turned into numbers here.
unamb[[5]] <- as.numeric(unamb[[5]])
 
# Sort the rows by sentence number. sentence_id looks like "unamb10", so "unamb"
# is cut off and what is left ("10") is read as a number.
unamb <- unamb[order(as.numeric(sub("unamb", "", unamb$sentence_id))), ]
 
# Save as a new file, with _unamb added to the name
write_csv(unamb, file.path(out_dir, paste0(base_name, "_unamb.csv")))
 
# -----------------------------------------------------------------------------------------------------------------------------------------------------------
 
# Unambiguous sentences, part 2: when each word was spoken
 
file <- "/documents/xxxx_pre_processed_unamb.csv" #using the file created in step 1
out_dir   <- dirname(file)
base_name <- tools::file_path_sans_ext(basename(file))
 
unamb <- read_csv(file)
 
# The sentence number of each row: "unamb10" becomes 10
sent_num <- as.numeric(sub("unamb", "", unamb$sentence_id))
 
# Two checks before going any further. Every number has to be between 1 and 20, and no sentence may turn up twice. The script stops if either fails.
if (any(is.na(sent_num)) || any(sent_num < 1 | sent_num > 20)) stop("sentence_id'lerde beklenmeyen değer var!")
if (anyDuplicated(sent_num)) stop("Aynı sentence_id birden fazla kez geçiyor!")
 
 
# When the second preposition starts in each sentence, in ms counted from the start of the audio. The list runs unamb1 to unamb20, so a sentence's number is
# also its place in the list.
onset_values <- c(
  2160, 2205, 2260, 2000, 2240, 2250, 2170, 2140, 2130, 2210,
  2030, 1950, 2130, 1890, 2070, 2385, 2350, 2050, 2020, 2180
)
 
# The same moment, but on the recording's own clock, so that it can be compared with the gaze timestamps. Looked up by sentence number, not by the
# row's position in the file.
unamb$roi_onset <- unamb$audio_start_global + onset_values[sent_num]
 
shift <- 200   # ms, added to every onset below, this is to compensate the saccade delay
 
unamb$roi_onset_shifted <- unamb$roi_onset + shift
 
 
# When each word starts, again in ms from the start of the audio. Each row is
# one word (W1 to W11), and each column is one sentence (unamb1 to unamb20).
word_onsets <- rbind(
  c(280, 260, 270, 280, 280, 270, 280, 260, 270, 270, 270, 280, 280, 280, 270, 280, 270, 260, 270, 270),
  c(500, 500, 470, 500, 510, 500, 470, 500, 500, 500, 500, 510, 480, 500, 500, 490, 500, 490, 500, 500),
  c(590, 640, 670, 620, 680, 590, 670, 620, 610, 620, 580, 660, 610, 630, 600, 580, 590, 590, 620, 600),
  c(990, 1110, 1190, 1010, 1020, 1110, 1150, 980, 1140, 1240, 1050, 990, 990, 920, 1040, 1220, 1140, 1100, 1020, 1130),
  c(1240, 1310, 1380, 1200, 1240, 1320, 1370, 1190, 1360, 1460, 1230, 1220, 1160, 1140, 1200, 1400, 1350, 1270, 1230, 1300),
  c(1350, 1420, 1470, 1300, 1350, 1410, 1470, 1300, 1460, 1530, 1340, 1320, 1260, 1250, 1300, 1500, 1450, 1360, 1340, 1400),
  c(1510, 1590, 1640, 1460, 1500, 1580, 1650, 1450, 1640, 1750, 1560, 1510, 1420, 1430, 1450, 1640, 1600, 1530, 1530, 1550),
  c(1570, 1670, 1740, 1560, 1580, 1660, 1760, 1510, 1740, 1860, 1630, 1610, 1480, 1530, 1530, 1770, 1720, 1620, 1620, 1660),
  c(2160, 2205, 2260, 2000, 2240, 2250, 2170, 2140, 2130, 2210, 2030, 1950, 2130, 1890, 2070, 2385, 2350, 2050, 2020, 2180),
  c(2410, 2500, 2540, 2290, 2540, 2510, 2440, 2400, 2390, 2510, 2310, 2190, 2380, 2140, 2340, 2670, 2610, 2290, 2290, 2450),
  c(2520, 2620, 2650, 2380, 2640, 2610, 2560, 2530, 2510, 2620, 2410, 2330, 2490, 2250, 2430, 2790, 2720, 2360, 2420, 2520)
)
 
 
# Turn those into the columns W1_onset ... W11_onset, on the recording's clock
for (w in 1:11) {
  unamb[[paste0("W", w, "_onset")]] <- unamb$audio_start_global + word_onsets[w, sent_num]
}
 
# The window closes 1500 ms after the last word starts
unamb$roi_onset_end <- unamb$W11_onset + shift + 1500
 
write_csv(unamb, file.path(out_dir, paste0(base_name, "_gaze.csv")))
 
 
 
# Unambiguous sentences, part 3: where the participant looked
 
library(jsonlite)
library(readr)
 
file <- "/documents/xxxx_pre_processed_unamb_gaze.csv" # using the file created in step 2
 
unamb <- read_csv(file, show_col_types = FALSE)
 
 
# Target count: how many gaze samples landed on the target, trial by trial:
target_counts <- c()
 
for (i in 1:nrow(unamb)) {
 
  locations <- fromJSON(unamb$init_locations[i]) #  locations = which quadrant each image was in, read out of the init_locations column
 
  target <- unamb$target_image[i]
 
  target_location <- locations[basename(locations$src) == target, ]
 
  target_quadrant <- target_location$quadrant #  target_quadrant = the quadrant of the target, found by its file name
 
  gaze <- fromJSON(unamb$gaze_samples[i]) #  gaze = the gaze recording, read out of the gaze_samples column; every sample has a time t and a quadrant
 
  #  gaze_after_prep  = only the samples inside the window, from the shifted preposition onset to the end of the window
  gaze_after_prep <- gaze[gaze$t > unamb$roi_onset_shifted[i] & gaze$t < unamb$roi_onset_end[i], ]
 
  count <- sum(gaze_after_prep$quadrant == target_quadrant) #  count = how many of those samples fell in the target's quadrant
 
  target_counts[i] <- count
}
 
unamb$target_count <- target_counts
 
 
# Distractor count: the same count again, this time for the distractor
 
distractor_counts <- c()
 
for (i in 1:nrow(unamb)) {
 
  locations <- fromJSON(unamb$init_locations[i])
 
  distractor <- unamb$distractor[i]
 
  distractor_location <- locations[basename(locations$src) == distractor, ]
 
  distractor_quadrant <- distractor_location$quadrant
 
  gaze <- fromJSON(unamb$gaze_samples[i])
 
  gaze_after_prep <- gaze[gaze$t > unamb$roi_onset_shifted[i] & gaze$t < unamb$roi_onset_end[i], ]
 
  count <- sum(gaze_after_prep$quadrant == distractor_quadrant)
 
  distractor_counts[i] <- count
}
 
unamb$distractor_count <- distractor_counts
 
 
# Target proportion:
 
total_gaze_counts <- c()
target_gaze_proportions <- c()
 
for (i in 1:nrow(unamb)) {
 
  gaze <- fromJSON(unamb$gaze_samples[i])
 
  gaze_after_prep <- gaze[gaze$t > unamb$roi_onset_shifted[i] & gaze$t < unamb$roi_onset_end[i], ]
 
  total_count <- nrow(gaze_after_prep) #total_count = every sample inside the window, wherever it landed
 
  # proportion  = the target count divided by that total. If no samples were recorded at all, the trial is left empty rather than dividing by zero.
  proportion <- if (total_count > 0) unamb$target_count[i] / total_count else NA
 
  total_gaze_counts[i] <- total_count
  target_gaze_proportions[i] <- proportion
}
 
unamb$total_gaze_count <- total_gaze_counts
unamb$target_gaze_proportion <- target_gaze_proportions
 
 
# Distractor proportion: the same share, this time for the distractor
 
distractor_gaze_proportions <- c()
 
for (i in 1:nrow(unamb)) {
 
  gaze <- fromJSON(unamb$gaze_samples[i])
 
  gaze_after_prep <- gaze[gaze$t > unamb$roi_onset_shifted[i] & gaze$t < unamb$roi_onset_end[i], ]
 
  total_count <- nrow(gaze_after_prep)
 
  proportion <- if (total_count > 0) unamb$distractor_count[i] / total_count else NA
 
  distractor_gaze_proportions[i] <- proportion
}
 
unamb$distractor_gaze_proportion <- distractor_gaze_proportions
 
 
# Proportions word by word: the same shares again, but one per word instead of one for the whole window:
# A word with no gaze samples in its window keeps its empty value.
 
shift <- 200   # ms
 
for (w in 1:11) {
  unamb[[paste0("W", w, "_target_prop")]]     <- NA
  unamb[[paste0("W", w, "_distractor_prop")]] <- NA
}
 
for (i in 1:nrow(unamb)) {
 
  # Which quadrant the target and the distractor were in on this trial
  locations <- fromJSON(unamb$init_locations[i])
  target_quadrant     <- locations$quadrant[basename(locations$src) == unamb$target_image[i]]
  distractor_quadrant <- locations$quadrant[basename(locations$src) == unamb$distractor[i]]
 
  gaze <- fromJSON(unamb$gaze_samples[i])
 
  for (w in 1:11) {
 
    start <- unamb[[paste0("W", w, "_onset")]][i] + shift # start = that word's onset, plus the 200 ms shift
 
    if (w < 11) {
      end <- unamb[[paste0("W", w + 1, "_onset")]][i] + shift
    } else {
      end <- start + 1500 # end = the next word's onset, plus the same shift. The last word has no next word, so its window is 1500 ms long.
    }
 
    # The gaze samples recorded while this word was being spoken
    gaze_word <- gaze[gaze$t >= start & gaze$t < end, ]
    total <- nrow(gaze_word)
 
    if (total > 0) {
      target_prop     <- sum(gaze_word$quadrant == target_quadrant) / total
      distractor_prop <- sum(gaze_word$quadrant == distractor_quadrant) / total
 
      unamb[[paste0("W", w, "_target_prop")]][i]     <- target_prop
      unamb[[paste0("W", w, "_distractor_prop")]][i] <- distractor_prop
    }
  }
}
 
unamb$shift_ms <- shift   # the shift is written into the file as a record
 
 
# Save over the same file
write_csv(unamb, file)
