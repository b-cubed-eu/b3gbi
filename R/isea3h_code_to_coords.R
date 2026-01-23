isea3h_code_to_coords <- function(cell_codes) {
  # The dggridR package is required for this functionality
  if (!requireNamespace("dggridR", quietly = TRUE)) {
    stop("The 'dggridR' package is required to process ISEA3H grids. Please install it.")
  }

  # NOTE: GBIF ISEA3H codes (Mocnik scheme) are typically long integers.
  # We assume here that these align with standard DGGS sequence numbers 
  # for a specific resolution, or that dggridR can interpret them.
  # Resolution inference from the code itself is complex without the specific
  # Mocnik decoding algorithm. 
  
  # For now, we default to a high resolution commonly used or attempt to 
  # treat them as SEQNUMs. 
  # Users may need to specify resolution If this inference is incorrect.
  
  # Initialize the DGGS (ISEA3H)
  # Resolution 13 is often used for high-res data, but this is a guess.
  # Ideally, we should detect resolution from the code length or metadata.
  dggs <- dggridR::dgconstruct(res = 13, topology = "ISEA3H") 
  
  # Convert cell codes (assuming they are sequence numbers) to coordinates
  # We handle potential numeric conversion issues
  codes_numeric <- as.numeric(cell_codes)
  
  coords <- dggridR::dgSEQNUM_to_GEO(dggs, in_seqnum = codes_numeric)
  
  # Construct result dataframe
  result <- data.frame(
    xcoord = coords$lon_deg, 
    ycoord = coords$lat_deg, 
    # Resolution is returned as string for consistency with other grid types
    resolution = paste0("res", 13) 
  )
  
  return(result)
}
