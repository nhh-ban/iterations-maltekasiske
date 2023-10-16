transform_metadata_to_df <- function(metadata_list) {
  # Extract the trafficRegistrationPoints list
  traffic_points_list <- metadata_list$trafficRegistrationPoints
  
  # Create an empty data frame to store the results
  result_df <- data.frame()
  
  # Iterate over each element in the traffic points list
  for (point in traffic_points_list) {
    # Check if all required fields are present and non-empty
    if (!is.null(point$id) &&
        !is.null(point$name) &&
        !is.null(point$latestData) &&
        !is.null(point$latestData$volumeByHour) &&
        !is.null(point$location) &&
        !is.null(point$location$coordinates) &&
        !is.null(point$location$coordinates$latLon) &&
        !is.null(point$location$coordinates$latLon$lat) &&
        !is.null(point$location$coordinates$latLon$lon)) {
      
      # Extract data from the current point
      id <- point$id
      name <- point$name
      latestData <- point$latestData$volumeByHour
      lat <- point$location$coordinates$latLon$lat
      lon <- point$location$coordinates$latLon$lon
      
      # Create a data frame for the current point
      point_df <- data.frame(id, name, latestData, lat, lon)
      
      # Append the point data frame to the result data frame
      result_df <- rbind(result_df, point_df)
    }
  }
  
  # Convert the latestData column to POSIXct in UTC
  result_df$latestData <- as.POSIXct(result_df$latestData, tz = "UTC")
  
  return(result_df)
}

to_iso8601 <- function(dt, offset) {
  dt <- as_datetime(dt) + days(offset)
  dt <- format(dt, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  return(dt)
}

transform_volumes <- function(json_response) {
  # Extract the relevant information from the JSON response
  volume_data <- json_response$trafficData$volume$byHour$edges
  
  # Create an empty data frame to store the results
  result_df <- data.frame()
  
  # Iterate over each data point in volume_data
  for (edge in volume_data) {
    from <- edge$node$from
    volume <- edge$node$total$volumeNumbers$volume
    
    # Create a data frame for the current data point
    data_point <- data.frame(from, volume)
    
    # Append the data point to the result data frame
    result_df <- rbind(result_df, data_point)
  }
  
  # Convert 'from' column to POSIXct format with UTC timezone
  result_df$from <- as.POSIXct(result_df$from, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  
  return(result_df)
}

