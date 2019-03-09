#This function allows you to compute d from dz (Morris & DeShon, 2002)

d_to_dz <- function(d, r){
  dz <- d/(sqrt(2*(1-r)))
  invisible(list(d = d,
                 r = r,
                 dz = dz))
}