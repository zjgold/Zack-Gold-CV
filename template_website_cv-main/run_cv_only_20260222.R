# Metadata ---------------------------------------------------------------------
# Personal Website
# Developed by Emily Markowitz, for myself
# 2026/02/22

# Directions -------------------------------------------------------------------
# simply run this ONE line of code to get your website!
# source("./run.R")

# Notes
# 1. Check that your gitignore includes your data folder, so it only shares what you want it to show. 
# 2. Make copies of your own bio and cv_data google drive files and start filling them in. 
# 3. Define yourname and yourwebsitelink
# 4. Rename pages as you like, but you must have an index and an about page. 

# Knowns -----------------------------------------------------------------------

yourname <- "Zachary Gold" 
yournames <- c(yourname, 
               "Gold, Zachary") # e.g., for paper authorships, as listed in cv_data.xlsx

# Fine tuned editing

title_ital <- c( # things that should always be italized in titles. 
  "Cum Laude")
desc_ital <- c("et al.") # things that should always be italized in descriptions. 
desc_bullet_ital <- c("STUDY SPP")
# Libraries --------------------------------------------------------------------

PKG <- c(
  # page management
  "rmarkdown",
  "pagedown",
  
  # tidyverse
  "dplyr",
  "glue",
  "magrittr",
  "readr",
  "readxl",
  
  # working with URLS
  "xml2", # check urls
  
  # mapping
  "leaflet",
  "leafpop",
  "maps", 
  
  # google drive
  "googledrive",
  "googlesheets4", # seems redundant, but maybe I'll get rid of this later
  "readtext", # for reading docx files
  
  # icons
  "fontawesome" #devtools::install_github("rstudio/fontawesome")
)

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE)}
}



# Sign into google drive -------------------------------------------------------

googledrive::drive_deauth()
googledrive::drive_auth()


# Load Data --------------------------------------------------------------------

# How to find the google drive ID of a document
# Google Drive File ID is a unique identifier of the file on Google Drive. File IDs are stable throughout the lifespan of the file, even if the file name changes.
# 
# To locate the File ID, right-click on the name of the file, choose the Get Shareable Link option, and turn on Link Sharing if needed.
# 
# You will see the link with a combination of numbers and letters at the end, and what you see after `id =`  is the File ID.
# https://drive.google.com/open?id=***ThisIsFileID***
#   
# If your file is already open in a browser, you can obtain File ID from its link:
# https://docs.google.com/spreadsheets/d/***ThisIsFileID***/edit#gid=123456789 


# EXAMPLE Spreadsheet with CV data
# https://docs.google.com/spreadsheets/d/1fj0-LgxIgHC9qprjDfoyFqOUtUcWGMbMS28mCWf6as8
source("cv/functions_cv.R")
cv_data <- create_CV_object(
  data_location = "https://docs.google.com/spreadsheets/d/1aQ6uimV8HuQzxfnM3-O8-et5gOhBUgdqrf7inTPfffM/edit?gid=0#gid=0",
  cache_data = FALSE)

dat0 <- cv_data$entries_data

# Edit Data --------------------------------------------------------------------


# Check links
# checkLinks(cv$img)
# checkLinks(cv$url)
# checkLinks(cv$url1)

# Make formatting edits that are difficult to keep in a CSV

dat0 <- dat0 %>%
  dplyr::mutate(public_cv = as.logical(public_cv)) %>%
  dplyr::mutate(website = as.logical(website)) %>%
  dplyr::mutate(custom_cv = as.logical(custom_cv)) %>%
# Images
  dplyr::mutate(images = ifelse(is.na(img), "",
                              paste0("![*",img_txt,"*](",img,"){width='400px'}"))) %>%
  # URL links
  dplyr::mutate(Links = ifelse(is.na(url), "",
                               paste0("[", url_txt,"](",url,")"))) %>%
  dplyr::mutate(Links = ifelse(is.na(url1), Links,
                               paste0(Links, " \n\n [", url_txt1,"](",url1,")")))

dat0$description_1 <- copyedit(
  format = "bold",
  pattern = c(yournames),
  x = dat0$description_1)

dat0$description_1 <- copyedit(
  format = "italics",
  pattern = desc_ital,
  x = dat0$description_1)

dat0$title <- copyedit(
  format = "italics",
  pattern = title_ital,
  x = dat0$title)

# Clean up entries dataframe to format we need it for printing
dat0 <- dat0 %>%
  tidyr::unite(
    tidyr::starts_with('description'),
    col = "description_bullets",
    sep = "\n- ",
    remove = FALSE,
    na.rm = TRUE
  ) %>%
  dplyr::mutate(description_bullets =
                   ifelse(description_bullets != "",
                          paste0("\n- ", description_bullets),
                          "")) %>%
  dplyr::mutate(timeline =
                  dplyr::case_when(
                    grepl(pattern = "[a-zA-Z]+", x = start) ~ start, # in review, in prep
                    is.na(start) ~ "",
                    is.na(end) ~ as.character(start),
                    start == end ~ as.character(start),
                    !is.na(start) & !is.na(end) ~
                      paste0(start, " - ", end)) ) %>%
  dplyr::arrange(desc((end))) %>%
  dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .))

dat0$description_bullets <- copyedit(
  format = "italics",
  pattern = desc_bullet_ital,
  x = dat0$description_bullets)

dat0 <- dat0 %>%
  dplyr::mutate(Links_inline =
                  ifelse(Links == "",
                         "",
                         paste0('Links: ', gsub(pattern = ' \n\n ',
                                     replacement = ', ', x = Links), ''))) %>%
  dplyr::arrange(desc(start))

# dat0$description_1[dat0$page %in% "papers"] <- NA
# dat0$description_2[dat0$page %in% "papers"] <- NA


cv_data$entries_data <- dat0

# Render CV --------------------------------------------------------------------

# package up data for use in CV!
source("cv/functions_cv.r")
readr::write_rds(cv_data, 'cv/cached_positions.rds')
cache_data <- TRUE

# Knit the HTML version
rmarkdown::render("cv/cv.rmd",
                  params = list(pdf_mode = FALSE,
                                cache_data = cache_data),
                  output_file = "index.html")


# Knit the PDF version to temporary html location
tmp_html_cv_loc <- fs::file_temp(ext = ".html")
rmarkdown::render("cv/cv.rmd",
                  params = list(pdf_mode = TRUE, cache_data = cache_data),
                  output_file = tmp_html_cv_loc)

# Convert to PDF using Pagedown
pagedown::chrome_print(input = tmp_html_cv_loc,
                       output = "docs/cv.pdf")




