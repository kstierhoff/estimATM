# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(googledrive, here)

# Copy plotBio to Google Drive
drive_upload(here::here("Doc/plotBio_Web.html"), overwrite = TRUE,
             path = as_dribble("https://drive.google.com/drive/u/1/folders/1swWdmcan16tR5Yj71Py_F9TdWMKdkI8b"))

drive_upload(here::here("Doc/plotBio_All.html"), overwrite = TRUE,
             path = as_dribble("https://drive.google.com/drive/u/1/folders/1swWdmcan16tR5Yj71Py_F9TdWMKdkI8b"))

# Copy checkTrawls to Google Drive
drive_upload(here::here("Doc/checkTrawls.html"), overwrite = TRUE,
             path = as_dribble("https://drive.google.com/drive/u/1/folders/1swWdmcan16tR5Yj71Py_F9TdWMKdkI8b"))

# Copy plotBio image(s) to Google Drive
drive_upload(here::here("Figs/fig_nasc_cufes_haul_wt.png"), overwrite = TRUE,
             path = as_dribble("https://drive.google.com/drive/u/1/folders/1swWdmcan16tR5Yj71Py_F9TdWMKdkI8b"))
