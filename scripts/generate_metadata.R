
#write a function to generate the metadata for the images to be used in the portflio
generate_metadata <- function(category, path_prefix = "images", output_dir = "data") {
  
  #construct the full path to the image directory (e.g., "images/category")
  img_dir <- fs::path(path_prefix, category)
  
  #list all image files in the directory with extensions jpg, jpeg, png, or webp
  img_files <- list.files(img_dir, pattern = "\\.(jpg|jpeg|png|webp)$", ignore.case = TRUE)
  
  #create a data frame (metadata table) with columns for each image
  metadata <- data.frame(file = img_files, #filename (e.g., image_1.jpg)
                         title = tools::file_path_sans_ext(img_files), #filename without extension (used as a default title)
                         location = "", #placeholder for image location (to be filled out manually)
                         date = "", #placeholder for date (to be filled out manually)
                         caption = "", #placeholder for caption (to be filled out manually)
                         stringsAsFactors = FALSE) #keep all text columns as character, not factors
  
  #create the output folder (e.g., "data") if it doesn't already exist
  if (!dir.exists(output_dir)) dir.create(output_dir)
  
  #construct the full output path (e.g., "data/marine_metadata.csv")
  output_file <- file.path(output_dir, paste0(category, "_metadata.csv"))
  
  #write the metadata data frame to a CSV file (without row numbers)
  write.csv(metadata, output_file, row.names = FALSE)
  
  #print a message to the console confirming the file was saved
  message("Metadata saved to: ", output_file)
}
