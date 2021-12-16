# ignore case for dplyr join
# https://gist.github.com/jimhester/a060323a05b40c6ada34

insensitive <- function(fun = inner_join) {
  new_fun <- fun
  body(new_fun) <- substitute({
    by <- dplyr:::common_by(by, x, y)
    
    tmp_by_x <- paste0("_", by$x, "_")
    tmp_by_y <- paste0("_", by$y, "_")
    for (i in seq_along(by$x)) {
      x[[tmp_by_x[[i]]]] <- tolower(x[[by$x[[i]]]])
      y[[tmp_by_y[[i]]]] <- tolower(y[[by$y[[i]]]])
      y[[by$y[[i]]]] <- NULL
    }
    
    res <- fun(x, y, list(x = tmp_by_x, y = tmp_by_y))
    res[tmp_by_x] <- list(NULL)
    
    res
  })
  
  new_fun
}       

df <- tibble(
  x = sample(10, 100, rep = TRUE),
  y = sample(10, 100, rep = TRUE)
)
nrow(distinct(df))
m <- distinct(df, .keep_all = TRUE)

