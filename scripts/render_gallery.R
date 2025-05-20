

#write a function to render an HTML photo gallery from a folder of images
render_gallery <- function(category, metadata_file = NULL, path_prefix = "images") {
  
  #build the full path to the image directory (e.g., "images/category")
  img_dir <- file.path(path_prefix, category)
  
  #list all image files in the directory with extensions jpg, jpeg, png, or webp
  images <- list.files(img_dir, pattern = "\\.(jpg|jpeg|png|webp)$", ignore.case = TRUE)
  
  #initialize metadata as NULL
  metadata <- NULL
  
  #if a metadata file is provided and it exists, read it in
  if (!is.null(metadata_file) && file.exists(metadata_file)) {
    metadata <- readr::read_csv(metadata_file, show_col_types = FALSE)
  }
  
  #begin the HTML container div for the masonry layout
  cat("<div class='masonry'>\n")
  
  #loop over each image file in the directory
  for (img in sample(images)) {
    
    #normalize filename
    img_clean <- tolower(trimws(img))
    
    #find the appropriate row using the file name 
    row <- if (!is.null(metadata)) metadata[metadata$file == img_clean, ] else NULL
    
    #build the image caption using title, location, date, and caption (if available)
    if (!is.null(row) && nrow(row) > 0) {
      desc <- glue::glue("<strong>{row$caption}</strong><br>{row$location}<br><em>{row$date}</em>")
      alt_text <- row$caption
    } else {
      desc <- ""
      alt_text <- img_clean
    }
    
    
    #construct the full path to the image 
    full_path <- file.path(path_prefix, category, img)

    #fancybox-compliant HTML
    cat(glue::glue(
      "<a href='{full_path}' data-fancybox='{category}' data-caption=\"{desc}\">
         <img src='{full_path}' alt='{alt_text}' />
       </a>\n"
    ))
  }
  
  #close the masonry layout container
  cat("</div>\n")
}
