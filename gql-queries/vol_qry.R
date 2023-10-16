vol_qry <- function(id, from, to) {
  query <- glue::glue('
    {{
      trafficData(trafficRegistrationPointId: "{id}") {{
        volume {{
          byHour(from: "{from}", to: "{to}") {{
            edges {{
              node {{
                from
                to
                total {{
                  volumeNumbers {{
                    volume
                  }}
                }}
              }}
            }}
          }}
        }}
      }}
    }}
  ')
  
  # Fetch the station name and set it as a global variable
  station_name <- stations_metadata_df$name[stations_metadata_df$id == id]
  assign("station_name", station_name, envir = .GlobalEnv)
  
  return(query)
}
