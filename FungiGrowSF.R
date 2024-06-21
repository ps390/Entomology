# Load necessary packages
library(jpeg)
library(sf)
library(lwgeom)

# Function to calculate the area of an sf polygon
calculate_polygon_area <- function(polygon) {
  st_area(polygon)
}

# Function to calculate the intersection length of a line with a polygon
calculate_intersection_length <- function(polygon, axis_line) {
  intersection <- st_intersection(polygon, axis_line)
  if (length(intersection) == 0) {
    return(0)
  } else {
    return(st_length(intersection))
  }
}

# Function to get the intersection points of a line with a polygon
get_intersection_points <- function(polygon, axis_line) {
  intersection <- st_intersection(polygon, axis_line)
  return(st_coordinates(intersection))
}

# Function to select points, draw a line, draw a Cartesian coordinate system, and a polygon
select_points_and_draw_cartesian_and_polygon <- function(image_path) {
  # Read the image
  img <- readJPEG(image_path)
  
  # Get image dimensions
  img_height <- nrow(img)
  img_width <- ncol(img)
  
  # Open a new plotting window with the same size as the image
  x11(width = img_width / 96, height = img_height / 96)  # Assuming 96 DPI for screen resolution
  
  # Plot the image
  plot(1:2, type='n', xlab='', ylab='', xlim=c(0, img_width), ylim=c(img_height, 0))
  rasterImage(img, 0, img_height, img_width, 0)
  
  # Allow the user to click on two points for the coordinate system
  points <- locator(2)
  
  # Plot the points
  points(x = points$x, y = points$y, col = 'red', pch = 19)
  
  # Draw a line between the two points
  lines(x = points$x, y = points$y, col = 'blue', lwd = 2)
  
  # Calculate the direction vector for the line
  dx <- points$x[2] - points$x[1]
  dy <- points$y[2] - points$y[1]
  
  # Calculate the length of the line
  line_length <- sqrt(dx^2 + dy^2)
  
  # Calculate unit vectors for the x-axis and y-axis
  unit_x <- c(dx, dy) / line_length
  unit_y <- c(-unit_x[2], unit_x[1])  # Perpendicular to the unit_x
  
  # Determine the length for the coordinate system axes (equal to image dimensions)
  axis_length <- max(img_width, img_height)
  
  # Calculate the end points for the x-axis and y-axis
  x_axis_pos <- c(points$x[2] + axis_length * unit_x[1], points$y[2] + axis_length * unit_x[2])
  x_axis_neg <- c(points$x[2] - axis_length * unit_x[1], points$y[2] - axis_length * unit_x[2])
  y_axis_pos <- c(points$x[2] + axis_length * unit_y[1], points$y[2] + axis_length * unit_y[2])
  y_axis_neg <- c(points$x[2] - axis_length * unit_y[1], points$y[2] - axis_length * unit_y[2])
  
  # Draw the x-axis (in green)
  lines(c(points$x[2], x_axis_pos[1]), c(points$y[2], x_axis_pos[2]), col = 'green', lwd = 2)
  lines(c(points$x[2], x_axis_neg[1]), c(points$y[2], x_axis_neg[2]), col = 'green', lwd = 2)
  
  # Draw the y-axis (in orange)
  lines(c(points$x[2], y_axis_pos[1]), c(points$y[2], y_axis_pos[2]), col = 'orange', lwd = 2)
  lines(c(points$x[2], y_axis_neg[1]), c(points$y[2], y_axis_neg[2]), col = 'orange', lwd = 2)
  
  # Allow the user to click multiple points for the polygon
  polygon_points <- locator(type = "p", n = 12)
  
  # Close the polygon by connecting the last point to the first
  polygon_points$x <- c(polygon_points$x, polygon_points$x[1])
  polygon_points$y <- c(polygon_points$y, polygon_points$y[1])
  
  # Create an sf object for the polygon
  polygon_sf <- st_polygon(list(cbind(polygon_points$x, polygon_points$y)))
  polygon_sf <- st_sfc(polygon_sf, crs = NA_crs_)  # Planar CRS
  
  # Draw the polygon
  plot(polygon_sf, add = TRUE, border = 'purple', lwd = 2, col = rgb(0.5, 0, 0.5, 0.5))
  
  # Create an sf object for the y-axis
  y_axis_line <- st_linestring(matrix(c(y_axis_neg[1], y_axis_neg[2], y_axis_pos[1], y_axis_pos[2]), ncol = 2, byrow = TRUE))
  y_axis_line <- st_sfc(y_axis_line, crs = NA_crs_)  # Planar CRS
  
  # Split the polygon with the y-axis
  split_polygons <- st_split(polygon_sf, y_axis_line)
  split_polygons <- st_collection_extract(split_polygons, "POLYGON")
  
  # Extract left and right polygons
  if (length(split_polygons) == 2) {
    left_polygon <- split_polygons[[1]]
    right_polygon <- split_polygons[[2]]
    
    # Check the positions of the centroids to determine which is left and which is right
    centroid_left <- st_centroid(left_polygon)
    centroid_right <- st_centroid(right_polygon)
    
    if (st_coordinates(centroid_left)[1] > points$x[2]) {
      temp <- left_polygon
      left_polygon <- right_polygon
      right_polygon <- temp
    }
    
    # Draw the left polygon in blue and the right polygon in red
    plot(left_polygon, add = TRUE, border = 'blue', lwd = 2, col = rgb(0, 0, 1, 0.5))
    plot(right_polygon, add = TRUE, border = 'red', lwd = 2, col = rgb(1, 0, 0, 0.5))
    
    # Create an sf object for the x-axis
    x_axis_line <- st_linestring(matrix(c(x_axis_neg[1], x_axis_neg[2], x_axis_pos[1], x_axis_pos[2]), ncol = 2, byrow = TRUE))
    x_axis_line <- st_sfc(x_axis_line, crs = NA_crs_)  # Planar CRS
    
    # Calculate the intersection lengths of the x-axis with the left and right polygons
    left_x_length <- calculate_intersection_length(left_polygon, x_axis_line)
    right_x_length <- calculate_intersection_length(right_polygon, x_axis_line)
    
    # Get the intersection points of the x-axis with the left and right polygons
    left_intersection_points <- get_intersection_points(left_polygon, x_axis_line)
    right_intersection_points <- get_intersection_points(right_polygon, x_axis_line)
    
    # Draw the intersection lines from the origin to the intersection points
    if (nrow(left_intersection_points) > 0) {
      for (i in 1:nrow(left_intersection_points)) {
        lines(c(points$x[2], left_intersection_points[i, 1]), c(points$y[2], left_intersection_points[i, 2]), col = 'pink', lwd = 3, lty = 1)
      }
    }
    
    if (nrow(right_intersection_points) > 0) {
      for (i in 1:nrow(right_intersection_points)) {
        lines(c(points$x[2], right_intersection_points[i, 1]), c(points$y[2], right_intersection_points[i, 2]), col = 'pink', lwd = 3, lty = 1)
      }
    }
    
  } else {
    left_polygon <- NULL
    right_polygon <- NULL
    left_x_length <- 0
    right_x_length <- 0
  }
  
  # Calculate the area of the original, left, and right polygons
  total_area <- calculate_polygon_area(polygon_sf)
  left_area <- if (!is.null(left_polygon)) calculate_polygon_area(left_polygon) else 0
  right_area <- if (!is.null(right_polygon)) calculate_polygon_area(right_polygon) else 0
  
  # Create a data frame to store the area and length values
  results_df <- data.frame(
    Total_Area = as.numeric(total_area),
    Left_Area = as.numeric(left_area),
    Right_Area = as.numeric(right_area),
    Left_X_Length = as.numeric(left_x_length),
    Right_X_Length = as.numeric(right_x_length)
  )
  
  # Create a list to store all spatial information
  spatial_info <- list(
    original_polygon = polygon_sf,
    left_polygon = left_polygon,
    right_polygon = right_polygon
  )
  
  # Return the results data frame and the spatial information
  return(list(results_df = results_df, spatial_info = spatial_info))
}

# Path to your image
image_path <- "C:/Users/49157/Desktop/personal data/Uni/Master/Sommersemester 2024/Forest Entomology/test_fungi.jpg"

# Call the function
polygon_info <- select_points_and_draw_cartesian_and_polygon(image_path)

# Print the results data frame
print("Results Data Frame:")
print(polygon_info$results_df)

# Print the spatial information
print("Spatial Information:")
print(polygon_info$spatial_info)