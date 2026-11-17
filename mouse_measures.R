
file <- ".../all_groups_mouse.csv"
df <- read_csv(file)        # correct trials only

# When each spoken sentence ends (ms from audio onset)
offsets <- read_csv(".../sentence_offsets.csv")

df <- df %>% left_join(offsets, by = "sentence_id")

# Any sentence that failed to match shows up here
df %>% filter(is.na(offset_ms)) %>% count(sentence_id)

# na_out and best_match are written here first, because trial_measures below uses them in every trial

# Empty row, used when a trial cannot be measured: one row with the same nine columns as a real result, but all of them empty (NA).
na_out <- tibble(MD = NA_real_, AUC = NA_real_, MD_abs = NA_real_,
                 AUC_abs = NA_real_, n_drag = NA_integer_,
                 match_err = NA_real_, pickup_dist = NA_real_,
                 early_drag = NA, drag_offset = NA_real_)

# Finds the drag: looks for the two points where the mouse moved the same way the correct image did (D)
best_match <- function(tr, D) {
  n <- nrow(tr)
  if (n < 3) return(NULL)
  ij  <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
  err <- sqrt(((tr$x[ij[, 2]] - tr$x[ij[, 1]]) - D[1])^2 +
                ((tr$y[ij[, 2]] - tr$y[ij[, 1]]) - D[2])^2)
  k <- which.min(err)
  if (length(k) == 0) return(NULL)
  list(i = ij[k, 1], j = ij[k, 2], err = err[k])
}


# Deviation measures (MD, AUC) for one trial, based on the path of the correct image only
trial_measures <- function(mouse_trajectory, quadrant_boundaries,
                           init_locations, final_locations,
                           correct_image, target_image, distractor,
                           audio_start_global, offset_ms) {

  # Nothing to measure without a trajectory, or without timing
  if (is.na(mouse_trajectory) || mouse_trajectory == "[]") return(na_out)
  if (is.na(audio_start_global) || is.na(offset_ms))       return(na_out)

  tr_full <- fromJSON(mouse_trajectory)              # screen coords
  area    <- fromJSON(quadrant_boundaries)$areaRect  # the free-sort arena
  init    <- fromJSON(init_locations)                # image positions before sorting
  final   <- fromJSON(final_locations)               # image positions after sorting

  cutoff <- audio_start_global + offset_ms   # the moment the sentence ends

  # Image positions are given inside the arena, so move them to screen coords
  init$x  <- init$x  + area$left
  init$y  <- init$y  + area$top
  final$x <- final$x + area$left
  final$y <- final$y + area$top

  # Where the correct image started, and where it ended up
  from <- init[ basename(init$src)  == basename(correct_image), ]
  to   <- final[basename(final$src) == basename(correct_image), ]
  if (nrow(from) != 1 || nrow(to) != 1) return(na_out)   # from and to should each hold exactly one row: where the correct image started, and where it ended up. 

  D <- c(to$x - from$x, to$y - from$y)   # how far the correct image moved

  # did the drag start before the sentence ended? the cursor is hidden but still works, so an early drag is possible so look at the whole trajectory first, before cutting anything off
  m_full <- best_match(tr_full, D) #drag
  early_drag  <- NA
  drag_offset <- NA_real_
  # m_full is only used when a match was found and it is close enough. 
  # err shows how good the match is: during the real drag the mouse and the image move together, so their movements should be almost identical. 
  # A large err means the matched points are not the drag, and the result is not used.
  if (!is.null(m_full) && m_full$err < 5) {  # !is.null(m_full) = is there a match, m_full$err < 5 = is it close enough to be trusted?
    # The first point of the matched drag is when the drag began. Subtracting cutoff gives the gap between that moment and the end of the sentence,
    # so a negative value means the drag started while the sentence was still playing.
    drag_offset <- tr_full$t[m_full$i] - cutoff   # negative = started too early
    early_drag  <- drag_offset < 0
  }

  # drop everything recorded before the sentence offset 
  tr <- tr_full[tr_full$t >= cutoff, ]
  # --- Trial-skipping checks ---
  # If fewer than five points are left after the cut, there is not enough of the path left to measure anything, so this trial is skipped.
  if (nrow(tr) < 5) {out <- na_out; out$early_drag <- early_drag; out$drag_offset <- drag_offset  # No measures can be taken here (out <- na_out; ...), but the trial must still appear in the
    # results table, so na_out is returned in their place. early_drag and drag_offset are written into it first, because they were known before the cut and the exclusion table needs them to show why it was dropped.
    return(out)
  }
  # If best_match comes back empty, the drag could not be found in the path, so this trial is skipped.
  m <- best_match(tr, D)
  if (is.null(m)) {out <- na_out; out$early_drag <- early_drag; out$drag_offset <- drag_offset
    return(out)
  }
  seg <- tr[m$i:m$j, ]   # the drag itself, everything between the two matched points
  # If the drag is made up of fewer than three points, it is just a straight line from one point to the next and has no curve to measure, so this trial is skipped.
  if (nrow(seg) < 3) {out <- na_out; out$early_drag <- early_drag; out$drag_offset <- drag_offset
    return(out)
  }
  # ---------------
  
  #  quality check: did the drag really start on top of the image?
  # free-sort images are 100x100 and coordinates mark the top-left corner
  pickup_dist <- max(abs(seg$x[1] - (from$x + 50)), abs(seg$y[1] - (from$y + 50)))   # seg is the stretch of the path we take to be the drag, so seg$x[1] and seg$y[1] are its first point. 
  #from is the image's top-left corner, and the images are 100x100, so from + 50 is the centre, 50 pixels from each edge. A result under 50 means the drag started on the image.

  # Resample the drag to 101 equally spaced time points: drags differ in how many points were recorded, so they cannot be compared as they are. 
  # Here each drag is cut into 101 evenly spaced points: point 1 is the start, point 101 is the end. All drags now have the same length.
  xn <- approx(seg$t, seg$x, n = 101, ties = mean)$y # x position over time
  yn <- approx(seg$t, seg$y, n = 101, ties = mean)$y # y position over time

  # Ideal path: from where the mouse actually grabbed the image, to the target
  off <- c(seg$x[1] - from$x, seg$y[1] - from$y)          # off = the distance from the image's top-left corner to the spot where the mouse clicked on it
  target  <- init[basename(init$src) == basename(target_image), ]  # target image  = where the mouse will be when the image is dropped on the target
  distractor_position  <- init[basename(init$src) == basename(distractor), ]  # distractor_point  = the same for the distractor; it shows which side the distractor is on
   # if either image is not found exactly once, the trial is skipped.
  if (nrow(target) != 1 || nrow(distractor_position) != 1) {
    out <- na_out; out$early_drag <- early_drag; out$drag_offset <- drag_offset
    return(out)
  }
  target_point <- c(target$x, target$y) + off
  distractor_point <- c(distractor_position$x, distractor_position$y) + off

  # The ideal path as a straight line
  dx <- target_point[1] - xn[1]; dy <- target_point[2] - yn[1]; L <- sqrt(dx^2 + dy^2)
  if (L < 1) {
    out <- na_out; out$early_drag <- early_drag; out$drag_offset <- drag_offset
    return(out)
  }

  # Area under the deviation curve - How far the mouse moved away from the straight line
  # The ideal path is a straight line from where the image was grabbed to thetarget. For each of the 101 points we note two things:
  # dev = how far the mouse was from the line
  # s = how far it had moved along the line
  # AUC is the area between the mouse's path and the line. To get it, the area is cut into 100 strips. step_width is each strip's width, dev is its height.
  dev  <- ((xn - xn[1]) * dy - (yn - yn[1]) * dx) / L   # distance off the ideal path
  s    <- ((xn - xn[1]) * dx + (yn - yn[1]) * dy) / L   # distance along the ideal path
  side <- sign((distractor_point[1] - xn[1]) * dy - (distractor_point[2] - yn[1]) * dx)  # + = towards the distractor
  step_width   <- abs(diff(s))
  auc <- function(v) sum(step_width * (head(v, -1) + tail(v, -1)) / 2)
  # tibble: a table in tidyverse - one row of results for this trial
  tibble(MD = max(dev * side), AUC = auc(dev * side),     
         MD_abs = max(abs(dev)), AUC_abs = auc(abs(dev)),
         n_drag = nrow(seg), match_err = m$err,
         pickup_dist = pickup_dist,
         early_drag = early_drag, drag_offset = drag_offset)
}

# Run the measures on every trial
res <- df %>%
  mutate(measures = pmap(
    list(mouse_trajectory, quadrant_boundaries, init_locations, final_locations, correct_image, target_image, distractor, audio_start_global, offset_ms),
    trial_measures
  )) %>%
  unnest(measures)

# A quick look before cleaning
summary(res$AUC); summary(res$match_err); summary(res$pickup_dist)
