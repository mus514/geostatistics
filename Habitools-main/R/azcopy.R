#' azcopy wrapper function to ease the use and transfer files to or from network drives
#'
#' @param net_drive_path network drive path (can point to a folder or a file)
#' @param local_path Path to local disk
#' @param nd2local Should the transfer be network disk --> local disk (TRUE) or local disk --> network disk (FALSE)
#' @param account Account information to connect to the drive (P drive is hnatprojets.file.core, S drive is hnatprojetsprive.file.core.)
#' @param container Container in which the data is stored (P drive is projet-et-database, S drive is fs-hnatprojetsprive)
#' @param SASkey SAS key to connect. Starts with sv... or ?sv (note that the '?' is added automatically)
#' @param drive Set information automatically for 'P' or 'S' drive, if KEYAZCPP (for P drive) and KEYAZCPS (for S drive) are set in .Renviron file (usethis::edit_r_environ()), automatically generates account, container and SASkey for P and S drive
#'
#' @returns A command that can be pasted in shell to run.
#' @export
#'
#' @examples \dontrun{
#' azcp(net_drive_path = "P:/Projets/Actif/importat_project_folder/3-Analyses",
#' local_path = "C:/Projects/importat_project_folder/*",
#' nd2local = FALSE,
#' account = 'hnatprojets.file.core',
#' container = 'projet-et-database',
#' SASkey = 'secret-key-here')
#' }
azcp <- function(net_drive_path, local_path, nd2local = FALSE, account, container, SASkey, drive = NULL) {

  if (!is.null(drive)) {
    switch(drive,
           P={
             account ='hnatprojets.file.core'
             container = 'projet-et-database'
             SASkey = Sys.getenv('KEYAZCPP')
           },
           S={
             account ='hnatprojetsprive.file.core'
             container = 'fs-hnatprojetsprive'
             SASkey = Sys.getenv('KEYAZCPS')
           },
           {
             message('Drive need to be specified with account, container, and SASkey \nUse `usethis::edit_r_environ()` to set KEYAZCPP and/or KEYAZCPS')
           }
    )
  }

  comm = 'azcopy copy' # command
  opts =  '--recursive=true' # options

  # Remove drive letter if it is present
  ndp_noLetter <- sub("^[A-Z]:/", "", net_drive_path)

  # Remove ? if it is present
  sKey <- sub("\\?", "", SASkey)

  # make the link to the network drive
  link_disk = sprintf("https://%s.windows.net/%s/%s?%s", account, container, ndp_noLetter, sKey)

  # Check which direction to send the data
  if (nd2local) {
    # From network drive to local disk
    commd = paste(comm, paste0('"',link_disk,'"'), paste0('"',local_path,'"'), opts)
  } else {
    # From local disk to  network drive
    commd = paste(comm, paste0('"',local_path,'"'), paste0('"',link_disk,'"'), opts)
  }
  # Print the command to paste in the terminal
  cat(commd)
}
