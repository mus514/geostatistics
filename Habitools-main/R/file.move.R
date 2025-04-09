#' Move files to another folder and rename them
#'
#' @param from File path to be moved from the original location
#' @param to Destination path and file name
#'
#' @returns Will only rename and/or move the file depending on the destination path and new file name if different from the file name of the 'from' argument.
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate a path to a file in temporary folder
#' f = file.path(tempdir(), c("original_file.txt"))
#' # Create the file
#' file.create(f)
#' # Move (rename) the file to another file name and/or location
#' file.move(from = f,
#'           to = file.path(tempdir(), c("new_file.txt")))
#' }
#' @source https://stackoverflow.com/questions/10266963/moving-files-between-folders
file.move <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(path = unique(todir), recursive=TRUE, showWarnings = FALSE)
  file.rename(from = from,  to = to)
}
