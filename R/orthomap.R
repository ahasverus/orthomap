#' @title Create an orthographic World map
#'
#' @description
#' ...
#'
#' @param ...
#'
#' @export
#'
#' @return ...
#'
#' @details ...
#'
#' @seealso \code{\link[maps]{map}}
#'
#' @examples
#' # ...

orthomap <- function(
  query      = NULL,
  centre     = c(0, 0),
  border     = NA,
  fill       = "#909090",
  grid       = TRUE,
  grid.color = "#969696",
  ...
) {


  ### Retrieve country centroid coordinates

  if (!is.null(query)) {

    countries <- maps::map('world', plot = FALSE)$names
    query     <- countries[grep(tolower(as.character(query)), tolower(countries))]

    if (length(query) == 0) {
      stop(paste("Unable to find", query, "in maps database."))
    }

    world  <- maps::map(region = query, plot = FALSE)
    centre <- c(mean(na.omit(world$y)), mean(na.omit(world$x)))
  }


  ### Get World country polygons coordinates (and labels)

  xy <- maps:::map.poly(
    database   = "world",
    regions    = ".",
    exact      = FALSE,
    xlim       = NULL,
    ylim       = NULL,
    boundary   = TRUE,
    interior   = TRUE,
    fill       = TRUE,
    as.polygon = TRUE
  )
  coord <- cbind(xy$x, xy$y)


  ### Project coordinates in orthographic

  d2r     <- pi / 180
  lat     <- coord[ , 2] * d2r
  long    <- coord[ , 1] * d2r
  cenlat  <- centre[1] * d2r
  cenlong <- centre[2] * d2r

  x     <- cos(lat) * sin(long - cenlong)
  y     <- cos(cenlat) * sin(lat) - sin(cenlat) * cos(lat) * cos(long - cenlong)
  front <- sin(cenlat) * sin(lat) + cos(cenlat) * cos(lat) * cos(long-cenlong) > 0

  coord <- cbind(coord, x, y, front)


  ### Find polygons delimitation

  naloc <- (1 : nrow(coord))[!complete.cases(coord)]
  naloc <- c(0, naloc, nrow(coord) + 1)


  ### Convert Matrix into SpatialPolygons

  polylist <- list()

  for (i in 2:length(naloc)) {

    thispoly <- coord[(naloc[i - 1] + 1):(naloc[i] - 1), 3:5, drop = FALSE]
    thispoly <- rbind(thispoly, thispoly[1, ])
    unq      <- unique(thispoly[ , 3])

    if (length(unq) == 1){

      if (unq == 1) { # Polygon is fully on front side

        polylist[[i - 1]] <- sp::Polygons(list(sp::Polygon(thispoly[ , 1:2])), as.character(i - 2))
      }

    } else { # Polygon is on front and back sides

      ind <- thispoly[ , 3] == 0

      # Project points "outside" the globe

      temdist <- pmax(sqrt(rowSums(as.matrix(thispoly[ind, 1:2] ^ 2))), 1e-5)
      thispoly[ind, 1 :2] <- thispoly[ind, 1:2] * (2 - temdist) / temdist

      polylist[[i - 1]] <- sp::Polygons(list(sp::Polygon(thispoly[ , 1:2])), as.character(i - 2))
    }
  }

  pos <- which(unlist(lapply(polylist, function(x) ifelse(is.null(x), 0, 1))) == 1)

  polylist <- polylist[pos]
  country  <- data.frame(
    country   = xy[[4]][pos],
    row.names = unlist(lapply(polylist, function(x) x@ID))
  )

  world <- sp::SpatialPolygonsDataFrame(
    Sr   = sp::SpatialPolygons(
      Srl         = polylist,
      proj4string = sp::CRS(paste0("+proj=ortho +lat_0=", centre[1], " +lon_0=", centre[2]))
    ),
    data = country
  )


  ### Create globe limits

  globe <- cbind(
    x = round(sin(seq(0, 2 * pi, length.out = 1000)), 10),
    y = round(cos(seq(0, 2 * pi, length.out = 1000)), 10)
  )

  globe <- sp::SpatialPolygons(
    Srl        = list(
      sp::Polygons(
        srl = list(
          sp::Polygon(
            globe[ , 1:2]
          )
        ),
        ID  = "0"
      )
    ),
    proj4string = sp::CRS(paste0("+proj=ortho +lat_0=", centre[1], " +lon_0=", centre[2]))
  )


  ### Clip polygons with globe

  world <- gBuffer(world, byid = TRUE, width = 0)
  globe <- gBuffer(globe, byid = TRUE, width = 0)
  world <- rgeos::gIntersection(world, globe, byid = TRUE, drop_lower_td = TRUE)
  world <- sp::spChFIDs(obj = world, x = as.character(1:length(world)))


  ### Plotting

  par(...)

  plot(world, col = fill, border = border)

  if (grid) {
    plot(globe, col = "transparent", border = grid.color, lwd = 0.25, add = TRUE)
    map.grid(labels = FALSE, lty = 1, col = grid.color, lwd = 0.25)
  }

  return(world)
}
