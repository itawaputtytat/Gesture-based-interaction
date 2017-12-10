METERS_DEGLON <- function(x) {

    d2r <- conv.deg2rad(x)
    
    result <- 
      (111415.13 * cos(d2r)) - 
      (94.55 * cos(3.0*d2r)) + 
      (0.12 * cos(5.0*d2r))
    
    return(result)
}


METERS_DEGLAT <- function(x) {
  
    d2r <- conv.deg2rad(x);
    
    result <- 
      (111132.09 - (566.05 * cos(2.0*d2r)) + 
         (1.20 * cos(4.0*d2r)) - 
         (0.002 * cos(6.0*d2r)));
}


conv.deg2rad <- function(x) {
  x <- x * pi/180
  return(x)
}




conv.gps2xy_byWHOI <- function(gps_lon,
                      gps_lat,
                      gps_lon_origin,
                      gps_lat_origin,
                      rotation_angle_deg = 0,
                      xoffset_m = 0,
                      yoffset_m = 0,
                      return_only_x = F,
                      return_only_y = F) {
  
  angle <- conv.deg2rad(rotation_angle_deg)
  
  xx = (gps_lon - gps_lon_origin) * METERS_DEGLON(gps_lat_origin);
  yy = (gps_lat - gps_lat_origin) * METERS_DEGLAT(gps_lat_origin);

  r = sqrt(xx*xx + yy*yy);

  #print(r)
  if(!is.nan(r)) {
  #if(r) {
      ct = xx/r;
      st = yy/r;
      xx = r * ( (ct * cos(angle)) + (st * sin(angle)) );
      yy = r * ( (st * cos(angle)) - (ct * sin(angle)) );
  }
  pxpos_mtrs = xx + xoffset_m;
  pypos_mtrs = yy + yoffset_m;
  
  if (return_only_x) {
    return(pxpos_mtrs)
  }
  
  if (return_only_y) {
    return(pypos_mtrs)
  }
  
  if (!return_only_x & !return_only_y) {
    return(list(x = pxpos_mtrs, y = pypos_mtrs))
  }
  
}


conv.gps2xy_byWHOI(11.62683, 48.07223,
          11.63825455, 48.07737816)

conv.gps2xy_byWHOI(11.64465, 48.07593,
          11.63825455, 48.07737816)
