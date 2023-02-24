library(readxl)
library(tidyverse)
library(lubridate)
library(exiftoolr)
library(camtrapR)

# Renaming of camera trap images####
setwd("F:/") #Set working directory

exiftoolPath("C:/Windows") #Define where your exiftool is
#for this to work you need to download exiftool separately and put it in that specifed directory

dat <- read.csv("MME_Camera_traps/Cameras/mme_stations_cameras.csv")

#please refer to camtrapr documentation to learn how to structure folders: renaming will happen based on folder structure and exif info

# Renaming code for Waste Point SITE

Renamed <- imageRename(inDir    = ("MME_Camera_traps/Cameras/Waste_Point"),

                       outDir    = ("MME_Camera_traps/renamed"),

                       hasCameraFolders = FALSE,
                       keepCameraSubfolders = FALSE,

                       copyImages = TRUE
                       )

#Renaming code for Chimney Ridge

Renamed <- imageRename(inDir    = ("MME_Camera_traps/Cameras/Chimney_Ridge"),

                       outDir    = ("MME_Camera_traps/renamed"),

                       hasCameraFolders = FALSE,
                       keepCameraSubfolders = FALSE,
                       copyImages = TRUE
                       )

#Renaming code for Mill Site

Renamed <- imageRename(inDir    = ("MME_Camera_traps/Cameras/Mill_Site"),

                       outDir    = ("MME_Camera_traps/renamed"),

                       hasCameraFolders = FALSE,
                       keepCameraSubfolders = TRUE,
                       copyImages = TRUE
                       )

# Extracting camera trap data from files####
setwd("C:/Users/sbon6737/Desktop/Tagging/tagged")
tagged_imgs <- ("C:/Users/sbon6737/Desktop/Tagging/tagged")
length(list.files(tagged_imgs, pattern = "JPG", recursive = TRUE))
dat <- read.csv("C:/Users/sbon6737/Desktop/Tagging/mme_stations_cameras.csv")
camera_data <- ("C:/Users/sbon6737/Desktop/Tagging/data_extracted")
# exifTagNames(inDir = "F:/MME_Camera_traps/tagged/CRCHE/")

data1 <- recordTable(inDir = tagged_imgs,
                     IDfrom = "metadata", #you can read species names from metadata or from directories
                     timeZone = "Australia/Brisbane",
                     metadataHierarchyDelimitor = "|",
                     metadataSpeciesTag = "Species1",
                     minDeltaTime = 0,#the number of minutes between independent events
                     deltaTimeComparedTo =  "lastIndependentRecord",
                     additionalMetadataTags = c("MoonPhase", "AmbientTemperature"),
                     exclude = c("Nil", "Unidentifiable", "Human"), #Excludes any tags that you specify
                     removeDuplicateRecords = FALSE,
                     returnFileNamesMissingTags = TRUE,
                     outDir = camera_data,
                     writecsv = FALSE)
writexl::write_xlsx(data1, path = "C:/Users/sbon6737/Desktop/Tagging/data_extracted/camtrapdata_WPMMNIL2.xlsx")


