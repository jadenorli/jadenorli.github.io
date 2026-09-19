# =============================================================================
# scripts/generate_metadata.R
#
# Defines ONE function, generate_metadata(), a MANUAL, RUN-ONCE utility. It
# scans an image folder and writes a blank CSV skeleton — one row per photo,
# with empty columns for you to fill in by hand.
#
# HOW IT FITS INTO THE SITE: it doesn't, at render time. This is scaffolding
# you run yourself in the R console when you add a new photo category, e.g.
#
#     source("scripts/generate_metadata.R")
#     generate_metadata("marine")
#
# ...which creates data/marine_metadata.csv with 83 empty rows. You then open
# that file in Excel or a text editor and type in the locations, dates and
# captions. render_gallery.R reads the finished CSV at render time.
#
# IMPORTANT — THIS FUNCTION IS NEVER CALLED BY THE SITE.
# photography.qmd contains `source("scripts/generate_metadata.R")`, which
# LOADS the function into memory but never invokes it. That source() line is
# dead weight on every render of that page and will be removed when we get to
# photography.qmd. The file itself stays — it's a genuinely useful tool, just
# not one the website needs.
#
# NOTHING IN THIS FILE HAS BEEN CHANGED. Comments expanded only. Your original
# comments are preserved below; mine add the reasoning behind each line and
# flag three things worth knowing (see the notes accompanying this file).
# =============================================================================


#write a function to generate the metadata for the images to be used in the portfolio
generate_metadata <- function(category,                 # Folder name AND filename stem
                                                        # for the output, e.g. "marine"
                                                        # reads images/marine/ and writes
                                                        # data/marine_metadata.csv.
                              path_prefix = "images",   # Where the image folders live.
                                                        # Matches render_gallery.R's
                                                        # default, so the two agree
                                                        # without being told to.
                              output_dir = "data") {    # Where the CSV goes. Also
                                                        # matches what the gallery pages
                                                        # pass to render_gallery().

  #construct the full path to the image directory (e.g., "images/category")
  img_dir <- fs::path(path_prefix, category)
                                    # fs::path() joins path segments — the fs package's
                                    # equivalent of base R's file.path().
                                    # NOTE: this is the only fs call in the file; the
                                    # output path below uses base R's file.path()
                                    # instead. The two do the same job here, so this is
                                    # a small inconsistency rather than a bug. It does
                                    # mean the file has a dependency on the fs package
                                    # for one line that base R could handle.
                                    # The `::` form means fs doesn't need to be attached
                                    # with library() first — it just needs to be
                                    # installed.

  #list all image files in the directory with extensions jpg, jpeg, png, or webp
  img_files <- list.files(img_dir, pattern = "\\.(jpg|jpeg|png|webp)$", ignore.case = TRUE)
                                    # Returns filenames only (not full paths), which is
                                    # what we want since the CSV stores bare filenames.
                                    # The pattern is a regular expression: a literal dot
                                    # (escaped as \\. in an R string), then one of the
                                    # four extensions, then end-of-string ($).
                                    # ignore.case=TRUE catches .JPG from a camera.
                                    # NOTE: this list is SHORTER than render_gallery.R's
                                    # equivalent, which also accepts gif, avif, tif and
                                    # tiff. So a .gif would be published by the gallery
                                    # but would never get a metadata row here. Worth
                                    # keeping in mind if you ever add an animated image.

  #create a data frame (metadata table) with columns for each image
  metadata <- data.frame(file = img_files,                          #filename (e.g., image_1.jpg)
                                    # THE JOIN KEY. render_gallery.R matches this column
                                    # against the actual filenames on disk, so it is the
                                    # one column that must never be edited by hand.
                         title = tools::file_path_sans_ext(img_files), #filename without extension (used as a default title)
                                    # FLAG: this column is never used. render_gallery.R
                                    # reads it into memory but never emits it into the
                                    # HTML — captions are built from caption, location
                                    # and date only. Harmless, but it's a column you
                                    # might waste time filling in.
                         location = "",                             #placeholder for image location (to be filled out manually)
                                    # A single "" is RECYCLED by data.frame() to fill
                                    # every row — that's R's vector recycling, not a
                                    # one-row table. Same for the two below.
                         date = "",                                 #placeholder for date (to be filled out manually)
                                    # Free text, not a real date. Your CSVs contain
                                    # values like "August, 2022", which render_gallery.R
                                    # prints verbatim in italics. Nothing parses it, so
                                    # any format works — but it also means nothing
                                    # catches a typo.
                         caption = "",                              #placeholder for caption (to be filled out manually)
                                    # The most important column: it becomes both the
                                    # bold lightbox caption AND the image's alt text.
                         stringsAsFactors = FALSE)                  #keep all text columns as character, not factors
                                    # HISTORICAL NOTE: before R 4.0.0 (2020),
                                    # data.frame() silently converted text to "factors"
                                    # (categorical codes), which broke string operations
                                    # in surprising ways. R 4.0+ defaults to FALSE, so
                                    # this line is now redundant — but it is good
                                    # defensive practice and costs nothing. Keep it.

  #create the output folder (e.g., "data") if it doesn't already exist
  if (!dir.exists(output_dir)) dir.create(output_dir)
                                    # NOTE: no recursive=TRUE here, unlike
                                    # render_gallery.R's dir.create(). With the default
                                    # output_dir of "data" that's fine — it's a single
                                    # level off the project root. But calling this with
                                    # something like output_dir = "data/galleries" would
                                    # fail unless data/ already existed.

  #construct the full output path (e.g., "data/marine_metadata.csv")
  output_file <- file.path(output_dir, paste0(category, "_metadata.csv"))
                                    # paste0() concatenates with no separator, so
                                    # "marine" + "_metadata.csv". This naming convention
                                    # is what lets the gallery pages predict the path.

  #write the metadata data frame to a CSV file (without row numbers)
  write.csv(metadata, output_file, row.names = FALSE)
                                    # row.names=FALSE suppresses R's automatic 1,2,3…
                                    # index column, which would otherwise appear as a
                                    # nameless first column and confuse readr on the
                                    # way back in.
                                    #
                                    # ***** THE ONE REAL HAZARD IN THIS FILE *****
                                    # write.csv OVERWRITES without asking. Running
                                    # generate_metadata("marine") today would replace
                                    # your 83 hand-written marine captions with 83 empty
                                    # rows, instantly and silently. There is no
                                    # confirmation prompt and no backup.
                                    # Since all four of your CSVs are already filled in,
                                    # this function should not be run again for an
                                    # existing category — only for a NEW one. See the
                                    # notes with this file for a two-line guard that
                                    # would make that mistake impossible.

  #print a message to the console confirming the file was saved
  message("Metadata saved to: ", output_file)
                                    # message() writes to stderr rather than stdout,
                                    # which is the R convention for status output — it
                                    # keeps this out of anything that captures results.
}
