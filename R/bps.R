BPS6E_URL <- "http://content.bfwpub.com/webroot_pubcontent/Content/BCS_5/BPS6e/Student/DataSets/Mac_Text/Mac_Text.zip"

#' Datasets in Basic Practice of Statistics 6th edition
#' 
#' List all the datasets included in the Basic Practice of Statistics, 6th edition.
#' 
#' @param chapter \code{integer} Chapter number.
#' @param data \code{character} Dataset name (excluding extension).
#' @return character vector of the names of the datasets.
#' @export
bps6e_data <- function(chapter, data) {
  # create a temporary directory
  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir=td, fileext=".zip")
  # download into the placeholder file
  download.file(BPS6E_URL, tf)
  unzip(tf, exdir = td)
  chapter_dir <- sprintf("Chapter %d", chapter)
  filename <- sprintf("%s.txt", data)
  read.delim(file.path(td, "Mac-Text", chapter_dir, filename),
             header = TRUE)
}

#' Datasets in Basic Practice of Statistics 6th edition
#' 
#' List all the datasets included in the Basic Practice of Statistics, 6th edition.
#' 
#' @param chapter integer. Chapter number. 
#' @return character vector of the paths of the datasets.
#' @export
bps6e_listdir <- function(chapter = NULL) {
  # create a temporary directory
  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir=td, fileext=".zip")
  # download into the placeholder file
  download.file(BPS6E_URL, tf)
  unzip(tf, exdir = td)
  if (! is.null(chapter)) {
    chapter_dir <- sprintf("Chapter %d", chapter)
    ret <- dir(file.path(td, "Mac-Text", chapter_dir))    
  } else {
    ret <- dir(file.path(td, "Mac-Text"), recursive = TRUE)
  }
  unname(sapply(ret, tools::file_path_sans_ext))
}

BPS5E_URL <- "http://bcs.whfreeman.com/bps5e/content/cat_030/PC-Text.zip"

#' Datasets in Basic Practice of Statistics 5th edition
#' 
#' List all the datasets included in the Basic Practice of Statistics, 5th edition.
#' 
#' @param chapter \code{integer} Chapter number.
#' @param data \code{character} Dataset name (excluding extension).
#' @return character vector of the names of the datasets.
#' @export
bps5e_data <- function(chapter, data) {
  # create a temporary directory
  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir=td, fileext=".zip")
  # download into the placeholder file
  download.file(BPS5E_URL, tf)
  unzip(tf, exdir = td)
  chapter_dir <- sprintf("ch%02d", chapter)
  filename <- sprintf("%s.dat", data)
  read.delim(file.path(td, "PC-Text", chapter_dir, filename),
                    header = TRUE)
}

#' Datasets in Basic Practice of Statistics 5th edition
#' 
#' List all the datasets included in the Basic Practice of Statistics, 5th edition.
#' 
#' @param chapter integer. Chapter number. 
#' @return character vector of the paths of the datasets.
#' @export
bps5e_listdir <- function(chapter = NULL) {
  # create a temporary directory
  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir=td, fileext=".zip")
  # download into the placeholder file
  download.file(BPS5E_URL, tf)
  unzip(tf, exdir = td)
  if (! is.null(chapter)) {
    chapter_dir <- sprintf("ch%02d", chapter)
    ret <- dir(file.path(td, "PC-Text", chapter_dir))    
  } else {
    ret <- dir(file.path(td, "PC-Text"), recursive = TRUE)
  }
  unname(sapply(ret, tools::file_path_sans_ext))
}




