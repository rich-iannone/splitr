#' @export
set_ascdata <- function(lat_lon_ll = c(-90.0, -180.0),
                        lat_lon_spacing = c(1.0, 1.0),
                        lat_lon_n = c(180, 360),
                        lu_category = 2,
                        roughness_l = 0.2,
                        data_dir = "'.'") {
  
  arg_names <- formals(set_ascdata) %>% names()
  arg_vals <- mget(arg_names)
  
  arg_vals$lat_lon_ll <- 
    paste0(arg_vals$lat_lon_ll[1], "  ", arg_vals$lat_lon_ll[2])
  
  arg_vals$lat_lon_spacing <- 
    paste0(arg_vals$lat_lon_spacing[1], "  ", arg_vals$lat_lon_spacing[2])
  
  arg_vals$lat_lon_n <- 
    paste0(arg_vals$lat_lon_n[1], "  ", arg_vals$lat_lon_n[2])
  
  arg_vals[!vapply(arg_vals, FUN = is.null, FUN.VALUE = logical(1))]
}

write_ascdata_list <- function(ascdata_list, dir) {
  
  paste0(ascdata_list, "\n", collapse = "") %>%
    cat(file = file.path(dir, "ASCDATA.CFG"))
}
