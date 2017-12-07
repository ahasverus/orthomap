#' @title Create a World map in orthographic projection
#'
#' @description
#' This function corrects a bug (some polygons are not correctly projected) in
#' the function \code{maps:map()} when it is used in orthographic projection. The
#' function \code{orthomap()} also returns projected polygons in the
#' \code{sp:SpatialPolygons} format.
#'
#' @param query The name of a country to center the map on.
#' @param centre Latitude and longitude of the map center. Ignored if query is not null.
#' @param border Color of polygons border.
#' @param fill Color of polygons.
#' @param grid If TRUE, grid (i.e. graticules) are added to the map.
#' @param nx Number of longitude lines.
#' @param ny Number of latitude lines.
#' @param grid.color Color of the grid.
#' @param grid.type Type of grid (see argument \code{lty}).
#' @param grid.size Size of grid (see argument \code{lwd}).
#'
#' @export
#'
#' @return A \code{SpatialPolygons} of world countries in orthographic projection without aberrations.
#'
#' @seealso \code{\link[maps]{map}}
#'
#' @examples
#' # See: https://github.com/ahasverus/orthomap


orthomap <- function(
  query      = NULL,
  centre     = c(0, 0),
  border     = NA,
  fill       = "#909090",
  grid       = TRUE,
  nx         = 10,
  ny         = 10,
  grid.color = "#969696",
  grid.type  = 1,
  grid.size  = 0.25,
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

      temdist <- pmax(sqrt(rowSums(as.matrix(thispoly[ind, 1:2]^2))), 0.00001)
      thispoly[ind, 1:2] <- thispoly[ind, 1:2] * (2 - temdist) / temdist

      polylist[[i - 1]] <- sp::Polygons(list(sp::Polygon(thispoly[ , 1:2])), as.character(i - 2))
    }
  }


  ### Delete outside polygons

  pos <- which(unlist(lapply(polylist, function(x) ifelse(is.null(x), 0, 1))) == 1)

  polylist <- polylist[pos]
  country  <- data.frame(
    country   = xy[[4]][pos],
    row.names = unlist(
      lapply(
        polylist,
        function(x) x@ID
      )
    )
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

  world <- rgeos::gBuffer( # To correct a bug in gIntersection
    spgeom = world,
    byid   = TRUE,
    width  = 0
  )

  globe <- rgeos::gBuffer( # To correct a bug in gIntersection
    spgeom = globe,
    byid   = TRUE,
    width  = 0
  )

  world <- rgeos::gIntersection(
    spgeom1       = world,
    spgeom2       = globe,
    byid          = TRUE,
    drop_lower_td = TRUE
  )

  world <- sp::spChFIDs(
    obj = world,
    x   = as.character(1:length(world))
  )


  ### Plot world map in ortho

  par(...)

  ooo <- options()$warn
  options(warn = -1)
  maps::map("world", proj = "orthographic", orient = c(centre, 0), col = NA)
  options(warn = ooo)

  sp::plot(
    world,
    col    = fill,
    border = border
  )

  if (grid) {

    sp::plot(
      globe,
      col    = "transparent",
      border = grid.color,
      lty    = grid.type,
      lwd    = grid.size,
      add    = TRUE
    )

    mapproj::map.grid(
      nx     = nx,
      ny     = ny,
      labels = FALSE,
      col    = grid.color,
      lty    = grid.type,
      lwd    = grid.size
    )
  }

  return(world)
}
