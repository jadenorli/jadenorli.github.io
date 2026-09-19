# =============================================================================
# scripts/render_gallery.R
#
# Defines ONE function, render_gallery(), which builds the HTML for a
# photography gallery and hands it back to Quarto to drop into the page.
#
# It is sourced and called by the four gallery pages:
#   photography-marine.qmd       category = "marine"
#   photography-terrestrial.qmd  category = "terrestrial"
#   photography-portraits.qmd    category = "portraits"
#   photography-microscopy.qmd   category = "microscope"   <- note: NOT
#                                  "microscopy". The folder, the CSV and the
#                                  argument all say "microscope" while the page
#                                  file says "microscopy". Not a bug — just a
#                                  naming inconsistency worth knowing about.
#
# WHAT IT DOES, end to end:
#   1. Lists the image files in images/<category>/
#   2. Reads data/<category>_metadata.csv for titles, locations, dates, captions
#   3. Copies each image into docs/ under a "web-safe" filename
#   4. Builds one <a><img></a> per photo, wrapped in <div class="masonry">
#   5. Returns that HTML so knitr prints it into the page verbatim
#
# ONE CODE CHANGE from the original, marked CHANGED in safe_name() below: the
# underscore was added to the set of characters the filename sanitizer leaves
# alone. Everything else is comments only.
#
# R NOTE FOR READING THIS FILE: `<-` is assignment (like `=` in most
# languages). `function(x) { ... }` defines a function. The last expression
# evaluated in a function is its return value — R has `return()` but rarely
# needs it. `pkg::name` calls a function from a package without attaching the
# whole package with library().
# =============================================================================

render_gallery <- function(category,                     # e.g. "marine". Names the
                                                         # image folder AND is used
                                                         # as the Fancybox album id.
                           metadata_file = NULL,         # Path to the CSV. NULL is
                                                         # allowed — the gallery just
                                                         # renders without captions.
                           path_prefix   = "images",     # where your originals live
                           output_dir    = "docs",       # web root used by GitHub Pages
                           randomize_mode = c("always","daily","off"),
                                                         # A vector of allowed values.
                                                         # This is R's idiom for an
                                                         # enum: match.arg() below
                                                         # picks the first as default
                                                         # and rejects anything not
                                                         # in the list.
                           exts = c("jpg","jpeg","png","webp","gif","avif","tif","tiff")) {
                                                         # File extensions to treat as
                                                         # images.
                                                         # FLAG: tif/tiff cannot be
                                                         # displayed by any browser. If
                                                         # a .tif ever lands in an image
                                                         # folder it will be copied and
                                                         # linked, and show as a broken
                                                         # image. Harmless today (no
                                                         # .tif files exist) but it is a
                                                         # trap waiting to be sprung.

  randomize_mode <- match.arg(randomize_mode)
                                    # Validates the argument against the vector above
                                    # and collapses it to a single string. Called with
                                    # no value, this yields "always". None of the four
                                    # gallery pages passes this argument, so all four
                                    # galleries shuffle on every page load.

  # ---- helpers -------------------------------------------------------------
  # Four small functions defined INSIDE render_gallery. They're local: they can
  # see render_gallery's arguments and they don't exist in your global R
  # environment after the call finishes. That's deliberate scoping, not an
  # accident — it keeps names like nfc() from colliding with anything else.

  # Normalize to UTF-8 + NFC (avoid macOS NFD surprises)
  nfc <- function(x) {
    stringi::stri_trans_nfc(iconv(as.character(x), "", "UTF-8", sub = ""))
                                    # THE PROBLEM THIS SOLVES: the character "é" can
                                    # be stored two ways — as one code point (NFC) or
                                    # as "e" plus a combining accent (NFD). macOS
                                    # filenames use NFD; almost everything else uses
                                    # NFC. Two strings that LOOK identical then fail
                                    # to compare equal, and metadata silently stops
                                    # matching its image.
                                    # iconv(..., sub="") first forces UTF-8 and drops
                                    # any byte it can't convert; stri_trans_nfc() then
                                    # composes everything to the NFC form.
                                    # This is why every string in this file passes
                                    # through nfc() before being compared or printed.
  }

  # Escape for HTML attributes
  esc_attr <- function(x) {
    x <- nfc(x)                                     # Normalize first (see above).
    x <- gsub("&",  "&amp;",  x, fixed = TRUE)      # MUST BE FIRST. Escaping & after
                                                    # the others would re-escape the
                                                    # ampersands they just introduced,
                                                    # turning &lt; into &amp;lt;.
    x <- gsub("<",  "&lt;",   x, fixed = TRUE)      # Prevents a caption containing <
    x <- gsub(">",  "&gt;",   x, fixed = TRUE)      # from being parsed as a tag.
    x <- gsub("\"", "&quot;", x, fixed = TRUE)      # Prevents a caption's double quote
                                                    # from terminating the attribute
                                                    # early and corrupting the HTML.
    x <- gsub("'",  "&#39;",  x, fixed = TRUE)      # Same for single quotes, which
                                                    # matter because some attributes
                                                    # below are single-quoted.
    x
                                    # fixed=TRUE means "treat the pattern as a literal
                                    # string, not a regular expression" — faster and
                                    # avoids any chance of a character being read as a
                                    # regex metacharacter.
                                    # WHY THIS MATTERS: your captions contain species
                                    # names and place names typed by hand. One
                                    # apostrophe in "Joe's Cave" would break the page
                                    # without this.
  }

  # Build a *web-safe* filename from an original name
  # (diacritics -> ASCII, punctuation -> -)
  safe_name <- function(fname) {
    ext  <- tolower(tools::file_ext(fname))                   # "JPG" -> "jpg"
    base <- tools::file_path_sans_ext(basename(fname))        # "images/a/b.jpg" -> "b"

    base <- stringi::stri_trans_nfc(base)                     # Normalize (see nfc()).
    base <- stringi::stri_trans_general(base, "Latin-ASCII")  # é → e, ñ → n, …
    base <- gsub("[’']", "", base)                            # drop apostrophes/quotes
    base <- gsub("[^A-Za-z0-9_]+", "-", base)                 # Any run of characters
                                                              # that isn't a letter, a
                                                              # digit or an UNDERSCORE
                                                              # becomes ONE hyphen.
                                                              # CHANGED (Aug 2026): the
                                                              # underscore was added to
                                                              # this set. Previously
                                                              # image_1.jpg was rewritten
                                                              # to image-1.jpg, so the
                                                              # copy below landed at a
                                                              # DIFFERENT path from the
                                                              # one Quarto's
                                                              # `resources: images/**`
                                                              # already publishes — every
                                                              # gallery photo ended up in
                                                              # docs/ twice, ~262 files.
                                                              # With the underscore
                                                              # allowed, safe_name() is a
                                                              # no-op for the current
                                                              # image_N.jpg naming, so the
                                                              # copy overwrites Quarto's
                                                              # file instead of sitting
                                                              # beside it. The sanitizing
                                                              # still protects against a
                                                              # future filename with
                                                              # spaces or accents.
                                                              # An underscore is safe in a
                                                              # URL and needs no
                                                              # percent-encoding, so
                                                              # allowing it costs nothing.
    base <- gsub("-+", "-", base)                             # Collapse runs of hyphens.
                                                              # Mostly redundant with the
                                                              # + in the line above, but
                                                              # catches hyphens that were
                                                              # already in the name.
    base <- gsub("(^-|-$)", "", base)                         # Trim a leading or
                                                              # trailing hyphen.

    paste0(tolower(base), ".", ext)                           # Reassemble, lowercased.
                                    # WHY THIS EXISTS AT ALL: GitHub Pages serves files
                                    # over HTTP, where spaces, accents and punctuation
                                    # in a filename must be percent-encoded and are a
                                    # common source of 404s. This sidesteps the problem
                                    # by guaranteeing every published filename is plain
                                    # lowercase ASCII.
  }

  # Join path but keep slashes; percent-encode unsafe chars segment-wise
  url_join <- function(...) {
                                    # `...` accepts any number of arguments. Called
                                    # below as url_join(path_prefix, category, img_web).
    segs <- lapply(list(...), function(s) {
      s <- gsub("\\\\", "/", s)                                # Convert Windows
                                                               # backslashes to forward
                                                               # slashes. (In R source,
                                                               # \\\\ is a regex matching
                                                               # one literal backslash.)
      parts <- strsplit(s, "/", fixed = TRUE)[[1]]             # Split into path
                                                               # segments. [[1]] because
                                                               # strsplit returns a list.
      paste(vapply(parts, utils::URLencode, "", reserved = TRUE), collapse = "/")
                                    # Percent-encode EACH SEGMENT separately, then
                                    # rejoin with "/". This is the whole point of
                                    # splitting: reserved=TRUE would otherwise encode
                                    # the slashes themselves into %2F and destroy the
                                    # path. vapply's "" argument declares that each
                                    # result is a single string — a type check that
                                    # makes the failure loud if something unexpected
                                    # comes back.
    })
    paste(segs, collapse = "/")     # Join the arguments together with slashes.
  }

  # ---- find originals ------------------------------------------------------
  img_dir   <- file.path(path_prefix, category)  # e.g. "images/terrestrial"
  pat       <- paste0("\\.(", paste(exts, collapse = "|"), ")$", collapse = "")
                                    # Builds a regular expression from the exts vector:
                                    # \.(jpg|jpeg|png|...)$ — "a dot, then one of these
                                    # extensions, then end of string". The outer
                                    # collapse= is a no-op here (the input is already
                                    # length 1) but is harmless.
  originals <- sort(list.files(img_dir, pattern = pat, ignore.case = TRUE))
                                    # Alphabetical list of matching filenames (names
                                    # only, not full paths). sort() makes the output
                                    # deterministic — important, because a stable order
                                    # here means the only randomness on the page comes
                                    # from the shuffle in site.js.
                                    # NOTE: alphabetical means image_10 sorts before
                                    # image_2. Irrelevant given the shuffle, but it
                                    # would matter if you ever turned shuffling off.

  if (!length(originals)) {
                                    # No images found — most likely a typo'd category
                                    # or a folder that hasn't been synced from iCloud.
    warning("No images found in ", img_dir)
                                    # Prints a warning during render. NOTE: the gallery
                                    # pages set `warning: false` in their YAML, so this
                                    # will NOT appear in the rendered page — but it does
                                    # appear in the R console when you render locally.
    return(knitr::asis_output("<div class='masonry'></div>\n"))
                                    # Return an empty gallery rather than erroring out,
                                    # so one bad folder doesn't break the whole site
                                    # build. Sensible defensive choice.
  }

  # ---- read metadata (robust UTF-8) ---------------------------------------
  meta <- NULL                      # Default: no metadata. Everything downstream
                                    # checks for NULL, so captions are optional.

  if (!is.null(metadata_file) && file.exists(metadata_file)) {
                                    # && short-circuits: if the first test fails, R
                                    # never evaluates the second. Required here —
                                    # file.exists(NULL) would error.

    # try to detect encoding, then normalize to NFC
    enc <- "UTF-8"                  # Assume UTF-8 unless told otherwise.
    ge  <- try(readr::guess_encoding(metadata_file, n_max = 2000), silent = TRUE)
                                    # Sniff the file's actual encoding from its first
                                    # 2000 lines. Wrapped in try() so that a failure
                                    # returns an error OBJECT instead of halting the
                                    # render.
                                    # WHY: a CSV edited in Excel on macOS can come out
                                    # as Latin-1 or UTF-16 rather than UTF-8, which
                                    # would turn every accented character into mojibake.
    if (!inherits(ge, "try-error") && nrow(ge) && !is.na(ge$encoding[1])) {
                                    # Three guards, in order: the call didn't error,
                                    # it returned at least one row, and that row's
                                    # encoding isn't missing. Only then trust it.
      enc <- ge$encoding[1]         # Use the best guess.
    }

    meta <- readr::read_csv(
      metadata_file,
      locale = readr::locale(encoding = enc),   # Read using the detected encoding.
      show_col_types = FALSE                    # Suppress readr's column-type report,
                                                # which would otherwise print noise on
                                                # every render.
    )

    stopifnot("file" %in% names(meta))
                                    # Hard requirement: the CSV must have a `file`
                                    # column, since that's the join key. stopifnot()
                                    # halts the render with a clear error if not —
                                    # deliberately louder than the warning above,
                                    # because a metadata file with the wrong shape is
                                    # a mistake you want to know about immediately.

    for (nm in c("file", "title", "location", "date", "caption")) {
      if (!nm %in% names(meta)) meta[[nm]] <- ""
                                    # Create any missing column as empty strings, so
                                    # the code below can reference all five
                                    # unconditionally. This is why portraits_metadata
                                    # .csv works fine despite having every location,
                                    # date and caption blank.
      meta[[nm]] <- nfc(meta[[nm]]) # Normalize each column (see nfc()).
    }

    # normalize matching keys: lower, NFC, ignore extension
    meta$.key <- tolower(tools::file_path_sans_ext(meta$file))
                                    # Build the join key: "image_1.JPG" -> "image_1".
                                    # Dropping the extension and lowercasing means the
                                    # CSV can say .jpg while the file is .JPG and they
                                    # still match.
                                    # The leading dot in `.key` is a convention marking
                                    # it as an internal working column, not data.
  }

  # ---- ensure copies exist under docs/ with safe names ---------------------
  dest_dir <- file.path(output_dir, path_prefix, category)  # docs/images/terrestrial
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
                                    # recursive=TRUE creates intermediate folders
                                    # (docs/ and docs/images/) if they're missing.
                                    # showWarnings=FALSE silences the complaint if
                                    # another process created it a moment earlier.
  }

  # map original -> web-safe copy
  map <- lapply(originals, function(f) {
                                    # lapply applies this function to every filename and
                                    # returns a list of results.
    src <- file.path(img_dir, f)    # Source: images/marine/image_1.jpg
    web <- safe_name(f)             # e.g., "sao-tome-osprey-reef.jpg"
    dst <- file.path(dest_dir, web) # Destination: docs/images/marine/image-1.jpg

    if (!file.exists(dst) || file.info(src)$mtime > file.info(dst)$mtime) {
                                    # Copy only if the destination is missing, or the
                                    # source has been modified more recently than the
                                    # copy. This is a hand-rolled incremental build —
                                    # it's what keeps a re-render from re-copying 262
                                    # photos every single time.
      ok <- try(file.copy(src, dst, overwrite = TRUE), silent = TRUE)
      if (inherits(ok, "try-error") || !isTRUE(ok)) {
                                    # Two failure modes checked: the call threw an
                                    # error, or it returned FALSE (file.copy signals
                                    # failure by return value, not by erroring).
        warning("Could not copy ", src, " -> ", dst)
                                    # Again suppressed in the page but visible in the
                                    # console. A permissions problem or an iCloud file
                                    # that hasn't downloaded would land here.
      }
    }

    list(orig = f, web = web)       # Return both names for this photo.
  })

  map <- do.call(rbind, lapply(map, as.data.frame))
                                    # Convert the list of two-element lists into a data
                                    # frame with one row per photo. do.call("rbind", x)
                                    # is the R idiom for "call rbind with every element
                                    # of x as a separate argument" — i.e. stack them.
  names(map) <- c("orig", "web")    # Name the columns explicitly rather than relying
                                    # on as.data.frame's inference.
  map$orig_key <- tolower(tools::file_path_sans_ext(nfc(map$orig)))
                                    # The join key on the image side, built exactly the
                                    # same way as meta$.key above so the two can be
                                    # compared. Note nfc() here and not there — the CSV
                                    # column was already normalized in the loop.

  # ---- build HTML ----------------------------------------------------------
  classes   <- paste("masonry", if (randomize_mode != "off") "randomize-on-load")
                                    # An if() with no else returns NULL when false, and
                                    # paste() silently drops NULL — so this yields
                                    # "masonry randomize-on-load" or just "masonry".
                                    # NOTE: nothing styles or reads .randomize-on-load.
                                    # It's a leftover hook. The actual shuffle is driven
                                    # by the data-randomize attribute below, which
                                    # scripts/site.js reads.
  data_attr <- if (randomize_mode != "off") {
    paste0(" data-randomize=\"", randomize_mode, "\"")
                                    # Emits ` data-randomize="always"`. The leading
                                    # space matters — it separates this attribute from
                                    # the preceding class attribute in the tag.
  } else {
    ""                              # Omit the attribute entirely when off, which is
                                    # what makes site.js skip the gallery.
  }

  out <- sprintf("<div class='%s'%s>\n", classes, data_attr)
                                    # Open the container. sprintf substitutes %s with
                                    # each argument in order. `out` accumulates the
                                    # whole HTML string as we loop.

  for (i in seq_len(nrow(map))) {
                                    # seq_len(n) is 1:n but safe when n is 0 — 1:0 would
                                    # give c(1,0) and run the loop backwards. A small
                                    # correctness habit worth copying.
    img_orig <- nfc(map$orig[i])    # Original filename, for the alt-text fallback.
    img_web  <- nfc(map$web[i])     # Published filename, for the URL.

    # find metadata row by key (case/diacritics insensitive, ignore extension)
    row <- NULL
    if (!is.null(meta)) {
      row <- meta[meta$.key == map$orig_key[i], , drop = FALSE]
                                    # Subset the metadata to rows whose key matches this
                                    # image. The empty slot after the comma means "all
                                    # columns"; drop=FALSE keeps the result a data frame
                                    # even when it has one row (without it R would
                                    # simplify it to a vector and row$caption would
                                    # fail).
      if (!nrow(row)) row <- NULL   # No match — treat as no metadata.
    }

    cap <- loc <- dat <- ""         # Chained assignment: all three start empty.
    if (!is.null(row)) {
      cap <- esc_attr(row$caption[1])   # [1] takes the first match, in case the CSV
      loc <- esc_attr(row$location[1])  # accidentally contains duplicate rows for one
      dat <- esc_attr(row$date[1])      # file. Silently ignores the rest.
    }

    parts <- character(0)           # An empty character vector to build up.
    if (nzchar(cap)) parts <- c(parts, sprintf("<strong>%s</strong>", cap))
                                    # nzchar() = "non-zero characters", i.e. not the
                                    # empty string. Only include a piece if it has
                                    # content — this is what stops the portraits
                                    # gallery (all fields blank) from rendering a
                                    # caption box full of stray <br> tags.
    if (nzchar(loc)) parts <- c(parts, loc)
    if (nzchar(dat)) parts <- c(parts, sprintf("<em>%s</em>", dat))
                                    # Caption bold, location plain, date italic.
                                    # These tags are added AFTER escaping, so they stay
                                    # live HTML while the user's text is inert. That
                                    # ordering is the whole security model here — worth
                                    # preserving if you ever edit this.
    desc <- nfc(paste(parts, collapse = "<br>"))
                                    # Join whichever pieces exist with line breaks.

    # URL relative to site root (docs/)
    rel_url <- url_join(path_prefix, category, img_web)
                                    # e.g. "images/marine/image-1.jpg". Relative, not
                                    # absolute — so the gallery works both on
                                    # jadenorli.github.io and when you open the file
                                    # locally.
    alt_txt <- if (nzchar(cap)) cap else tools::file_path_sans_ext(img_orig)
                                    # Alt text for screen readers and for when an image
                                    # fails to load. Prefers the real caption; falls
                                    # back to the bare filename.
                                    # ACCESSIBILITY NOTE: "image_1" is a poor
                                    # description. Every marine, terrestrial and
                                    # microscopy photo has a real caption so this rarely
                                    # fires — but all 39 portraits have blank captions,
                                    # so that entire gallery currently has filenames as
                                    # alt text. Filling in the portraits CSV would fix
                                    # it with no code change.

    out <- paste0(
      out,
      "<a href='", rel_url, "' data-fancybox='", esc_attr(category),
                                    # data-fancybox groups photos into an album — all
                                    # images sharing a value become one swipeable set
                                    # in the lightbox. Using the category name means one
                                    # album per gallery page.
      "' data-caption=\"", desc, "\">",
                                    # The lightbox caption. Note this attribute uses
                                    # DOUBLE quotes (escaped as \" in R) while href uses
                                    # single — deliberate, since desc contains <strong>
                                    # and <em> tags with no quotes, but could contain an
                                    # escaped &#39; from an apostrophe.
      "<img src='", rel_url, "' alt='", esc_attr(alt_txt),
      "' loading='lazy' decoding='async'>",
                                    # loading='lazy' defers downloading images until
                                    # they're near the viewport — essential on a page
                                    # with 108 photos. decoding='async' lets the browser
                                    # decode the image off the main thread so scrolling
                                    # stays smooth. Both are one-word performance wins.
      "</a>\n"
    )
  }

  out <- paste0(out, "</div>\n")    # Close the masonry container.
  Encoding(out) <- "UTF-8"          # Explicitly tag the finished string as UTF-8.
                                    # Belt-and-braces with the setup-utf8 chunk in each
                                    # gallery page — without it, accented characters in
                                    # captions can be mangled on the way into knitr.
  knitr::asis_output(out)
                                    # THE KEY LINE. asis_output() tells knitr "this is
                                    # already HTML, print it verbatim". Without it,
                                    # knitr would escape the angle brackets and you'd
                                    # see the literal tags as text on the page.
                                    # This is also why the calling chunks must specify
                                    # results='asis'.
}
