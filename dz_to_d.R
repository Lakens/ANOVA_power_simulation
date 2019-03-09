#This function allows you to compute dz from d (Morris & DeShon, 2002)

dz_to_d <- function(dz, r){
  d <- dz * sqrt(2*(1-r))
  invisible(list(d = d,
                 r = r,
                 dz = dz))
}