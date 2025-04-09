################################################################################
## Function to extract the categories of a raster layer based on quantile values
################################################################################
# date: 2025-03-21
# authors: Marc-Olivier Beausoleil
# Priorisation helper functions (useful in Zonation analyses)


#' Priority sites from Zonation layer
#'
#' @param r SpatRaster
#' @param probs numeric. Probability [0, 1] passed to `quantile` function. Note that if 2 values are provided to `probs`, `direction` will be set to 'both'
#' @param direction character or NULL. Direction to keep only highest ('high') values, lowest ('low') values or 'both'. If q has length >2 and dir Null, will combine different classes together (reclassify the raster based on categories)
#' @param verbose logical. If TRUE (default == FALSE), will print information about the Reclassification matrix when `direction = NULL`
#'
#' @description
#' When performing a Zonation analysis, the output is a raster layer with
#' continuous values [0, 1].
#' In some cases, it is required to categorize the Zonation output in the
#' lower 10% and upper 90% of the values in the raster to find the
#' regions with low and high priority.
#' The `topPrio` function assists in finding the regions based on
#' `quantile` probs arguments. For example, probs = c(0, 0.1, 0.9, 1) make three
#' categories: 1. between 0 and 0.1, 2. between 0.1 and 0.9, and
#' 3. between 0.9 and 1.
#'
#'
#' @returns A SpatRaster with the values corresponding to the quantile probability and the direction of value selection. Plots the data.
#' @export
#' @source https://stackoverflow.com/questions/72406230/how-to-select-top-30-of-cells-in-a-raster-by-cell-value-in-r
#'
#' @examples
#' r <- terra::rast(system.file("ex/elev.tif", package = "terra"))
#' # Exact extraction of the values above probs
#' r_tp1 = topPrio(r = r, probs = 0.9, direction = 'high')
#' terra::plot(r_tp1)
#' # Exact extraction of the values below probs
#' r_tp2 = topPrio(r = r, probs = 0.1, direction = 'low')
#' terra::plot(r_tp2)
#' # Reclass vals below/above probs
#' r_tp3 = topPrio(r = r, probs = c(0.1, 0.9))
#' terra::plot(r_tp3)
#' # Reclass vals below/above/ between probs
#' r_tp4 = topPrio(r = r, probs = c(0, 0.1, 0.9, 1), direction = NULL)
#' terra::plot(r_tp4)
#' \dontrun{
#' r_tp5 = topPrio(r = r, probs = c(0, 0.1, 0.9, 1), direction = NULL)
#' # Change the colour
#' rb = viridis::viridis(n=3)
#' terra::plot(r_tp5, col = c(rb[1], 'grey', rb[3]))
#' }
topPrio = function(r,
                   probs,
                   direction = NULL,
                   verbose = FALSE) {

  # Automatically assign direction if probs has length 2
  if(base::length(probs)==2){
    mssg = sprintf('length(probs)==%s, switching direction to "both"\n\n',
                   length(probs))
    cli::cli_alert_info(mssg)
    direction <- "both"
  } else if(base::length(probs)>2){
    direction <- NULL
  }

  # Get the values for a quantile
  q <- terra::global(x = r,
                     fun = \(i) quantile(x = i,
                                         probs = probs,
                                         na.rm = TRUE))

  # Prepare title for plot
  q_lab = paste(round(q, 2))

  # Depending on direction, select if keep the high or low values
  if (!is.null(direction)) {
    # For high
    if (direction == 'high') {
      # keep all values if the values are higher than threshold
      x <- terra::ifel(r <= q[[1]], NA, r)

    } else if (direction == 'low') {
      # keep all values if the values are lower than threshold
      x <- terra::ifel(r >= q[[1]], NA, r)

    } else if (direction == 'both' & length(q)==2) {
      # High values
      xh <- terra::ifel(r <= q[[2]], NA, r)
      x_high = terra::ifel(!is.na(xh), 2, xh)
      # Low values
      xl <- terra::ifel(r >= q[[1]], NA, r)
      x_low = terra::ifel(!is.na(xl), 1, xl)

      # Combine the layers low and high
      x = terra::mosaic(x_high, x_low)

      # Add levels low and high for legend
      levels(x) = data.frame(code = 1:2, lvl = c('Low', 'High'))

    }
  } # End if NOT null direction

  # If direction is NULL and more quantile categories are selected, generate all useful categories
  if (is.null(direction)) {
    if (length(probs)<=2) {
      cli::cli_abort(text = sprintf('You have length(probs) <= %d categories.
                                           Please, increase the number of `probs` categories or change the `direction`', length(probs)))
    }

    # unlist values
    q = unlist(q)

    # Generate matrix with rules to reclass all the quantile
    # Make range of values to generate the categories
    dfrcl = data.frame(from = q[-length(q)],
                       to = q[-1],
                       becomes = 1:(length(q)-1)) # reclass code
    # Make matrix
    m = as.matrix(dfrcl)

    # Print the reclassification matrix
    if (verbose) {
      cli::cli_alert_info("Reclassification matrix (round, 2 digits) ...")
      print(round(m, 2))
    }

    # raster with reclass
    x <- terra::classify(x = r,
                         rcl = m,
                         others = NA, # All other values = NA
                         include.lowest = TRUE, # value EQUAL to the LOWEST value in rcl should be included
                         right = TRUE) #intervals closed at the right and open on the left
    # Make levels (for legend) and format for pretty text
    levls_low = sprintf('[%s,%s]',
                        format(q[1], digits = 2),
                        format(q[2], digits = 2))
    levls_high = sprintf('(%s,%s]',
                         format(q[c(-1, -length(q))], digits = 2),
                         format(q[-c(1:2)], digits = 2))
    levls = c(levls_low, levls_high)
    # Add the levels to the raster
    levels(x) = data.frame(code = 1:(length(q)-1),
                           lvl = levls)
  } # End if NULL direction


  # Export the reclassified raster
  return(x)
}
