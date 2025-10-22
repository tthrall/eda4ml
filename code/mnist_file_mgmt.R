#####
###
#     mnist_file_mgmt.R
#
#       Document the downloading and processing of MNIST files.
###
#####

##
#  create_mnist_ve()
#     create the virtual environment 
#     required by keras::dataset_mnist()
#     
#     return the duration of each process
##
create_mnist_ve <- function() {
  # install_python()
  begin_py <- lubridate::now()
  reticulate::install_python()
  end_py <- lubridate::now()
  dur_py <- lubridate::time_length(end_py - begin_py)
  
  # install_tensorflow()
  begin_tf <- lubridate::now()
  tensorflow::install_tensorflow()
  end_tf <- lubridate::now()
  dur_tf <- lubridate::time_length(end_tf - begin_tf)
  
  # return process duration in seconds
  dur_tbl <- tibble::tibble(
    proc = paste0("install_", c("python", "tensorflow")), 
    secs = c(dur_py, dur_tf)
  )
  return(dur_tbl)
}

##
#  download_mnist_gz()
#    Download compressed MNIST component files.
#    Return in-memory list of components.
##
download_mnist_gz <- function() {
  mnist_lst <- keras::dataset_mnist()
  
  return(mnist_lst)
}

##
#  assert_mnist_gz_filenames()
#    names and sizes of compressed MNIST component files
##
assert_mnist_gz_filenames <- function() {
  fn_mnist_gz <- tibble::tibble(
    purpose = rep(c("train", "test"), each = 2L), 
    content = rep(c("images", "labels"), times = 2L), 
    f_name  = c(
      "train-images-idx3-ubyte.gz", 
      "train-labels-idx1-ubyte.gz", 
      "t10k-images-idx3-ubyte.gz", 
      "t10k-labels-idx1-ubyte.gz"
    ), 
    f_size    = c(9.5, 28.2, 1.6, 4.4), 
    size_unit = c("M", "K", "M", "K"))
  
  return(fn_mnist_gz)
}

##
#  a_3d_to_long_tbl()
#    Convert a 3D array to a long tibble.
##
a_3d_to_long_tbl <- function(
    x,          # <val> an array
    perm = NULL # <int> a permutation of (1:3)
  ) {
  # check dim(x)
  assertthat::assert_that(
    length(dim(x)) == 3L, 
    min(dim(x))    >= 2L
  )
  
  # transpose x as needed
  if (! is.null(perm)) {
    assertthat::assert_that(
      length(perm) == length(dim(x))
    )
    x <- aperm(x, perm = perm)
  }
  
  # the 3rd x-index varies most slowly and is listed first
  d <- dim(x)
  x_long <- tibble::tibble(
    idx_3 = 
      rep(1:d[[3]], each = d[[1]] * d[[2]]), 
    idx_2 = 
      rep(rep(1:d[[2]], each = d[[1]]), times = d[[3]]), 
    idx_1 = 
      rep(1:d[[1]], times = d[[2]] * d[[3]]), 
    value = x |> as.list() |> purrr::list_simplify()
  )
  
  return(x_long)
}

##
#  read_mnist_gz_images()
#    read an encoded, compressed MNIST image file
#    
#    return a 3D "image_array" with 
#    dim = (n_rows, n_cols, n_images)
##
read_mnist_gz_images <-  function(
    file # <chr> path to gzipped image file
) {
  # connect to file
  con <- gzfile(
    description = file, 
    open        = "rb")
  
  # read header (16 bytes for images)
  magic <- 
    readBin(con, "integer", n = 1, size = 4, endian = "big")
  n_images <- 
    readBin(con, "integer", n = 1, size = 4, endian = "big")
  n_rows <- 
    readBin(con, "integer", n = 1, size = 4, endian = "big")
  n_cols <- 
    readBin(con, "integer", n = 1, size = 4, endian = "big")
  
  # verify magic number
  if (magic != 2051L) {
    close(con)
    stop(paste("Invalid magic number", magic, "in", file))
  }
  
  # read all pixel data into integer vector "pixels"
  n_pixels <- n_images * n_rows * n_cols
  pixels <- 
    readBin(con, "integer", n = n_pixels, size = 1, signed = FALSE)
  close(con)
  
  # reshape "pixels" into 3D "image_array": 
  image_array <- array(pixels, dim = c(n_cols, n_rows, n_images))
  
  # transpose "image_array" so that 
  # dim = (n_rows, n_cols, n_images)
  # in order to create column-major ordering
  image_array <- aperm(image_array, c(2, 1, 3))
  
  return(image_array)
}

##
#  read_mnist_gz_labels()
#    read an encoded, compressed MNIST label file
#    
#    return an integer vector of labels, one per image
##
read_mnist_gz_labels <-  function(
    file # <chr> path to gzipped label file
) {
  # connect to file
  con <- gzfile(
    description = file, 
    open        = "rb")
  
  # read header (8 bytes for labels)
  magic <- 
    readBin(con, "integer", n = 1, size = 4, endian = "big")
  n_labels <- 
    readBin(con, "integer", n = 1, size = 4, endian = "big")
  
  # verify magic number
  if (magic != 2049L) {
    close(con)
    stop(paste("Invalid magic number", magic, "in", file))
  }
  
  # read all label data into integer vector "labels"
  labels <- 
    readBin(con, "integer", n = n_labels, size = 1, signed = FALSE)
  close(con)
  
  return(labels)
}

##
#  specify_mnist_subsets()
##
specify_mnist_subsets <- function(
    n_mnist_train = 10L^3L, # <int> desired number of training images
    n_mnist_test  = 10L^3L, # <int> desired number of test images
    train_suffix  = "_1K",  # <chr> filename suffix matching n_mnist_train
    test_suffix   = "_1K"   # <chr> filename suffix matching n_mnist_test
) {
  # set tibble names of component subsets
  mnist_subset_specs <- tibble::tibble(
    purpose = rep(c("train", "test"), each = 2L), 
    content = rep(c("images", "labels"), times = 2L), 
    nm_obj = c(
      paste0("mnist", train_suffix, "_train_images"), 
      paste0("mnist", train_suffix, "_train_labels"), 
      paste0("mnist", test_suffix,  "_test_images"), 
      paste0("mnist", test_suffix,  "_test_labels")
    ), 
    n_images = c(
      n_mnist_train, n_mnist_train, n_mnist_test, n_mnist_test) |> 
      as.integer()
  )
  
  return(mnist_subset_specs)
}

##
#  list_labeled_image_subsets()
#    return a subset of images and corresponding labels
##
list_labeled_image_subsets <- function(
    image_array, # <int> 3D array of pixels
    labels,      # <int> vector of labels matching image_array
    img_idx      # <int> vector of indices of desired images
) {
  # check dimensions of (image_array, labels)
  assertthat::assert_that(
    length(dim(image_array)) == 3L, 
    purrr::is_vector(labels), 
    length(labels) == dim(image_array) [3]
  )
  
  # check img_idx
  assertthat::assert_that(
    min(img_idx) >= 1L, 
    max(img_idx) <= dim(image_array) [3]
  )
  
  return(list(
    image_subset  = image_array [, , img_idx], 
    labels_subset = labels [img_idx]
  ))
}

##
#  write_vector()
#    Save an atomic vector to a specified file in a specified format.
#    
#    Return a list of output file paths.
##
write_vector <- function(
    x,                 # <val> atomic vector
    file,              # <chr> output file-path w/o file extension
    rds_lgl = FALSE,   # <lgl> save vector in RDS format
    tsv_lgl = TRUE,    # <lgl> save vector in TSV format
    col_name = "value" # <chr> column name within TSV file
) {
  # initialize file paths
  fp_sans  <- tools::file_path_sans_ext(file)
  file_rds <- ""
  file_txt <- ""
  
  # RDS format
  if (rds_lgl) {
    file_rds <- paste0(fp_sans, ".rds")
    x |> readr::write_rds(file = file_rds)
  }
  
  # TSV format
  if (tsv_lgl) {
    file_txt <- paste0(fp_sans, ".txt")
    x |> 
      tibble::as_tibble_col(column_name = col_name) |> 
      readr::write_tsv(file = file_txt)
  }
  
  output_path_lst <- list(
    file_rds = file_rds, 
    file_txt = file_txt)
  
  return(output_path_lst)
}

##
#  write_rds_array()
#    Save an atomic array as an RDS file.
#    
#    Return the output file path.
##
write_rds_array <- function(
    x,   # <val> atomic array
    file # <chr> output file-path w/o file extension
) {
  # set file path
  fp_sans  <- tools::file_path_sans_ext(file)
  file_rds <- paste0(fp_sans, ".rds")
  
  # write array x in RDS format
  x |> readr::write_rds(file = file_rds)
  
  output_path_lst <- list(file_rds = file_rds)
  
  return(output_path_lst)
}

##
#  write_df()
#    Save a data frame (or tibble) to a 
#    specified file in a specified format.
#    
#    Return a list of output file paths.
##
write_df <- function(
    df,                # <tbl> data frame or tibble
    file,              # <chr> output file-path w/o file extension
    rds_lgl = FALSE,   # <lgl> save df in RDS format
    tsv_lgl = TRUE     # <lgl> save df in TSV format
) {
  # initialize file paths
  fp_sans  <- tools::file_path_sans_ext(file)
  file_rds <- ""
  file_txt <- ""
  
  # RDS format
  if (rds_lgl) {
    file_rds <- paste0(fp_sans, ".rds")
    df |> readr::write_rds(file = file_rds)
  }
  
  # TSV format
  if (tsv_lgl) {
    file_txt <- paste0(fp_sans, ".txt")
    df |> 
      readr::write_tsv(file = file_txt)
  }
  
  output_path_lst <- list(
    file_rds = file_rds, 
    file_txt = file_txt)
  
  return(output_path_lst)
}

##
#  display_digit()
#    display a single digit, or more generally, 
#    a single specified image in a 3D array of images
##
display_digit <- function(
    image_array, # <int> 3D array of pixels
    labels,      # <int> vector of labels matching image_array
    img_idx      # <int> single index of desired image
) {
  # extract specified image from image_array
  img_mat <- image_array[, , img_idx]
  
  # display specified image
  graphics::image(
    img_mat [nrow(img_mat):1, ] |> t(), 
    col = gray.colors(256), 
    axes = FALSE,
    main = paste("Label: ", labels[img_idx])
  )
}


##
#  EOF
##
