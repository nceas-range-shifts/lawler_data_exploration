here_anx <- function(f = '', ...) {
  ### create file path to git-annex dir for project
  f <- paste(f, ..., sep = '/')
  f <- stringr::str_replace_all(f, '\\/+', '/')
  f_anx <- sprintf('/home/shares/usgs-cap-rangeshifts/%s', f)
  return(f_anx)
}
