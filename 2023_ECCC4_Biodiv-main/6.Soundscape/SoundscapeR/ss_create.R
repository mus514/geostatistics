ss_create <- function(fileloc,
                      samplerate,
                      window,
                      index = "CVR",
                      date,
                      lat,
                      lon,
                      method,
                      value,
                      output) {
  # 1. Merge the index files into a soundscape
  
  merged_soundscape <- ss_index_merge(
    fileloc = fileloc,
    samplerate = samplerate,
    window = window,
    index = index,
    date = date,
    lat = lat,
    lon = lon
  )
  
  cli::cli_alert_success("Merging of index files complete...")
  
  # 2. Binarize the index files
  
  if (missing(value)) {
    binarized_soundscape <- soundscapeR::ss_binarize(
      merged_soundscape = merged_soundscape,
      method = method
    )
  } else {
    binarized_soundscape <- soundscapeR::ss_binarize(
      merged_soundscape = merged_soundscape,
      method = method,
      value = value
    )
  }
  
  cli::cli_alert_success("Binarizing soundscape complete...")
  
  # 3. Aggregate the index files
  
  aggregated_soundscape <- soundscapeR::ss_aggregate(
    binarized_soundscape = binarized_soundscape,
    output = output
  )
  
  cli::cli_alert_success("Aggregating soundscape complete: a soundscape object has been created!")
  
  return(aggregated_soundscape)
}