#!/usr/bin/env Rscript
# update_book_chapter_los.R
#
# Updates Learning Objectives callout blocks in book chapter files
# using the authoritative learning-objectives-master.txt
#
# Usage:
#   1. Set paths in the Configuration section below
#   2. Source this script: source("update_book_chapter_los.R")
#   3. Call: update_all_chapters()      # updates all chapters
#      Or:   update_chapter(10)         # updates a single chapter
#      Or:   preview_chapter(10)        # preview without writing
#
# Requirements: dplyr, readr, stringr

library(dplyr)
library(readr)
library(stringr)

# ============================================================================
# Configuration - EDIT THESE PATHS
# ============================================================================

# Path to learning-objectives-master.txt (TSV format)
LO_MASTER_PATH <- here::here(
 "..", "eda4ml-instructor", "teaching-guides", "learning-objectives-master.txt"
)

# Path to book chapters directory
BOOK_CHAPTERS_DIR <- here::here()

# Chapter slug mapping (chapter number -> filename without .qmd)
CHAPTER_SLUGS <- c(
  "1"  = "eda",
 "2"  = "conditioning",
 "3"  = "clustering",
 "4"  = "simulation",
 "5"  = "study-design",
 "6"  = "info-theory",
 "7"  = "lin-reg",
 "8"  = "pca",
 "9"  = "lin-discr",
 "10" = "text-as-data",
 "11" = "topic-models",
 "12" = "ts-data",
 "13" = "ts-time-domain",
 "14" = "ts-freq-domain",
 "15" = "graph-theory"
)

# ============================================================================
# Functions
# ============================================================================

#' Read the LO master file
#' @return A tibble with columns: chapter, obj_idx, objective, in_book, in_workbook
read_lo_master <- function(path = LO_MASTER_PATH) {
 readr::read_tsv(
   path,
   col_types = readr::cols(
     chapter     = readr::col_integer(),
     obj_idx     = readr::col_integer(),
     objective   = readr::col_character(),
     in_book     = readr::col_logical(),
     in_workbook = readr::col_logical()
   ),
   show_col_types = FALSE
 )
}

#' Generate the LO callout block for a chapter
#' @param chapter_num Integer chapter number
#' @param lo_data Tibble from read_lo_master()
#' @return Character vector of lines for the callout block
generate_lo_block <- function(chapter_num, lo_data) {
 
 # Filter to book LOs for this chapter
 chapter_los <- lo_data |>
   dplyr::filter(chapter == chapter_num, in_book == TRUE) |>
   dplyr::arrange(obj_idx)
 
 if (nrow(chapter_los) == 0) {
   warning(paste("No book LOs found for chapter", chapter_num))
   return(NULL)
 }
 
 # Build the numbered list
 lo_lines <- paste0(chapter_los$obj_idx, ". ", chapter_los$objective)
 
 # Assemble the callout block
 block <- c(
   "::: {.callout-note}",
   "## Learning objectives",
   "",
   lo_lines,
   ":::"
 )
 
 return(block)
}

#' Get the file path for a chapter
#' @param chapter_num Integer chapter number
#' @return Character path to the .qmd file
get_chapter_path <- function(chapter_num) {
 slug <- CHAPTER_SLUGS[as.character(chapter_num)]
 if (is.na(slug)) {
   stop(paste("Unknown chapter number:", chapter_num))
 }
 file.path(BOOK_CHAPTERS_DIR, paste0(slug, ".qmd"))
}

#' Find and replace the LO callout block in chapter content
#' @param content Character vector of file lines
#' @param new_block Character vector of new callout block lines
#' @return Character vector with updated content
replace_lo_block <- function(content, new_block) {
 
 # Pattern to find the LO callout block
 # Matches from "::: {.callout-note}" followed by "## Learning objectives"
 # through the closing ":::"
 
 # Find start: line with "::: {.callout-note}" followed by "## Learning objectives"
 start_idx <- NULL
 for (i in seq_along(content)) {
   if (stringr::str_detect(content[i], "^:::\\s*\\{.callout-note\\}")) {
     # Check if next non-empty line is "## Learning objectives"
     for (j in (i+1):min(i+3, length(content))) {
       if (stringr::str_detect(content[j], "^##\\s*Learning [Oo]bjectives"))
         start_idx <- i
         break
       }
     }
   }
   if (!is.null(start_idx)) break
 }
 
 if (is.null(start_idx)) {
   warning("Could not find Learning objectives callout block")
   return(NULL)
 }
 
 # Find end: the closing ":::" for this block
 end_idx <- NULL
 for (i in (start_idx + 1):length(content)) {
   if (stringr::str_detect(content[i], "^:::$")) {
     end_idx <- i
     break
   }
 }
 
 if (is.null(end_idx)) {
   warning("Could not find closing ::: for Learning objectives block")
   return(NULL)
 }
 
 # Replace the block
 new_content <- c(
   content[1:(start_idx - 1)],
   new_block,
   content[(end_idx + 1):length(content)]
 )
 
 return(new_content)
}

#' Preview the updated LO block for a chapter (does not write)
#' @param chapter_num Integer chapter number
#' @export
preview_chapter <- function(chapter_num) {
 lo_data <- read_lo_master()
 block <- generate_lo_block(chapter_num, lo_data)
 
 cat("\n### Chapter", chapter_num, "- New LO Block ###\n\n")
 cat(block, sep = "\n")
 cat("\n")
 
 invisible(block)
}

#' Update a single chapter file
#' @param chapter_num Integer chapter number
#' @param dry_run If TRUE, show what would change without writing
#' @export
update_chapter <- function(chapter_num, dry_run = FALSE) {
 
 lo_data <- read_lo_master()
 new_block <- generate_lo_block(chapter_num, lo_data)
 
 if (is.null(new_block)) {
   return(invisible(FALSE))
 }
 
 chapter_path <- get_chapter_path(chapter_num)
 
 if (!file.exists(chapter_path)) {
   warning(paste("Chapter file not found:", chapter_path))
   return(invisible(FALSE))
 }
 
 # Read current content
 content <- readLines(chapter_path, warn = FALSE)
 
 # Replace the block
 new_content <- replace_lo_block(content, new_block)
 
 if (is.null(new_content)) {
   return(invisible(FALSE))
 }
 
 if (dry_run) {
   cat("\n### Chapter", chapter_num, "###\n")
   cat("File:", chapter_path, "\n")
   cat("Would update LO block to:\n\n")
   cat(new_block, sep = "\n")
   cat("\n")
 } else {
   # Write updated content
   writeLines(new_content, chapter_path)
   cat("Updated:", chapter_path, "\n")
 }
 
 invisible(TRUE)
}

#' Update all chapter files
#' @param chapters Integer vector of chapter numbers (default: all 15)
#' @param dry_run If TRUE, show what would change without writing
#' @export
update_all_chapters <- function(chapters = 1:15, dry_run = FALSE) {
 
 if (dry_run) {
   cat("=== DRY RUN - No files will be modified ===\n")
 }
 
 results <- sapply(chapters, function(ch) {
   update_chapter(ch, dry_run = dry_run)
 })
 
 cat("\nProcessed", sum(results), "of", length(chapters), "chapters\n")
 
 invisible(results)
}

#' Compare current chapter LOs to master
#' @param chapter_num Integer chapter number
#' @export
compare_chapter <- function(chapter_num) {
 
 lo_data <- read_lo_master()
 new_block <- generate_lo_block(chapter_num, lo_data)
 
 chapter_path <- get_chapter_path(chapter_num)
 
 if (!file.exists(chapter_path)) {
   warning(paste("Chapter file not found:", chapter_path))
   return(invisible(NULL))
 }
 
 content <- readLines(chapter_path, warn = FALSE)
 
 # Extract current block
 start_idx <- NULL
 end_idx <- NULL
 
 for (i in seq_along(content)) {
   if (stringr::str_detect(content[i], "^:::\\s*\\{.callout-note\\}")) {
     for (j in (i+1):min(i+3, length(content))) {
       if (stringr::str_detect(content[j], "^##\\s*Learning [Oo]bjectives")) {
         start_idx <- i
         break
       }
     }
   }
   if (!is.null(start_idx)) break
 }
 
 if (!is.null(start_idx)) {
   for (i in (start_idx + 1):length(content)) {
     if (stringr::str_detect(content[i], "^:::$")) {
       end_idx <- i
       break
     }
   }
 }
 
 if (is.null(start_idx) || is.null(end_idx)) {
   cat("Could not find LO block in chapter", chapter_num, "\n")
   return(invisible(NULL))
 }
 
 current_block <- content[start_idx:end_idx]
 
 # Compare
 current_str <- paste(current_block, collapse = "\n")
 new_str <- paste(new_block, collapse = "\n")
 
 if (current_str == new_str) {
   cat("Chapter", chapter_num, ": UP TO DATE\n")
 } else {
   cat("Chapter", chapter_num, ": NEEDS UPDATE\n")
   cat("\n--- Current ---\n")
   cat(current_block, sep = "\n")
   cat("\n\n--- New ---\n")
   cat(new_block, sep = "\n")
   cat("\n")
 }
 
 invisible(list(current = current_block, new = new_block))
}

#' Check all chapters against master
#' @param chapters Integer vector of chapter numbers
#' @export
check_all_chapters <- function(chapters = 1:15) {
 cat("=== Checking chapters against LO master ===\n\n")
 for (ch in chapters) {
   compare_chapter(ch)
 }
}

# ============================================================================
# Usage Examples (commented out)
# ============================================================================

# # Preview what the new LO block would look like
# preview_chapter(10)

# # See what would change (dry run)
# update_chapter(15, dry_run = TRUE)

# # Update a single chapter
# update_chapter(15)

# # Check all chapters against master
# check_all_chapters()

# # Update all chapters (dry run first!)
# update_all_chapters(dry_run = TRUE)
# update_all_chapters()

# # Update only chapters 10-15
# update_all_chapters(chapters = 10:15)

cat("update_book_chapter_los.R loaded.\n")
cat("Functions available: preview_chapter(), update_chapter(), update_all_chapters(),\n")
cat("                     compare_chapter(), check_all_chapters()\n")
cat("\nEdit LO_MASTER_PATH and BOOK_CHAPTERS_DIR in the Configuration section.\n")
