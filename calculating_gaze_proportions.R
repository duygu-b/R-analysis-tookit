install.packages("jsonlite")
library(jsonlite)
library(readxl)
library(writexl)
library(openxlsx)


file <- "/MyExperiments/1785328468671_undefined.xlsx" # a sample file
wb <- loadWorkbook(file) #using the excel file as a workbook instead of a data frame because it allowed me to modify the specific worksheet while preserving the workbook structure and other sheets
amb <- read.xlsx(wb, sheet = "amb") #reading the data in "amb" sheet

target_counts <- c()  #an empty vector to collect one computed number per trial as the loop runs

for (i in 1:nrow(amb)) {                    #The main loop to find the quadrant of target image
  # parsing JSON into R data frame
  locations <- fromJSON(amb$init_locations[i])    #extracting the initial locations of the Visual World images from a JSON string storing the starting positions of the images on screen and the screen quadrant it was placed in
  
  target <- amb$target_image[i]  # identifying the target image for the current trial
  
  target_location <- locations[basename(locations$src) == target, ] #extracting the image filename from the full path by using basename, and finding its location info
  
  target_quadrant <- target_location$quadrant  #extracting the target quadrant from the extracted location info
  
  #reading gaze JSON 
  gaze <- fromJSON(amb$gaze_transitions[i])  # parsing JSON into R data frame
  
  #taking the ones after preposition onset by checking the timestamps "t"
  gaze_after_prep <- gaze[gaze$t > amb$prep_onset[i], ]
  
  #calculating total number of gaze on target quadrant
  count <- sum(gaze_after_prep$quadrant == target_quadrant)
  
  target_counts[i] <- count  #storing the results in "target_counts" vector, one value per trial
}
#after the loop, new column is added to save target_count 
amb$target_count <- target_counts

removeWorksheet(wb, "amb")
addWorksheet(wb, "amb")
writeData(wb, "amb", amb)

#saving the modified file
saveWorkbook(wb, file, overwrite = TRUE)
