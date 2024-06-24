install.packages("magick")

library(magick)
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
  # Read the image using magick
  img <- image_read(image_path)
  img_info <- image_info(img)
  
  # Get image dimensions
  img_height <- img_info$height
  img_width <- img_info$width
  
  # Open a new plotting window with the same size as the image
  x11(width = img_width / 96, height = img_height / 96)  # Assuming 96 DPI for screen resolution
  
  # Plot the image
  plot(1:2, type='n', xlab='', ylab='', xlim=c(0, img_width), ylim=c(img_height, 0))
  rasterImage(as.raster(img), 0, img_height, img_width, 0)
  
  # Allow the user to click on two points to set the scale
  scale_points <- locator(2)
  
  # Plot the points
  points(x = scale_points$x, y = scale_points$y, col = 'red', pch = 19)
  
  # Draw a line between the two points
  lines(x = scale_points$x, y = scale_points$y, col = 'blue', lwd = 2)
  
  # Calculate the length of the scale line in pixels
  dx <- scale_points$x[2] - scale_points$x[1]
  dy <- scale_points$y[2] - scale_points$y[1]
  pixel_length <- sqrt(dx^2 + dy^2)
  
  # Ask the user to input the real-world length corresponding to the scale line (e.g., in cm)
  real_length <- as.numeric(1)
  
  # Calculate the scale factor (pixels per cm)
  scale_factor <- pixel_length / real_length
  
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
  polygon_points <- data.frame(x = numeric(0), y = numeric(0))
  cat("Click points to draw the polygon. Right-click to finish./n")
  while (TRUE) {
    new_point <- locator(1)
    if (is.null(new_point)) break
    polygon_points <- rbind(polygon_points, new_point)
    points(new_point$x, new_point$y, col = 'red', pch = 19)
    if (nrow(polygon_points) > 1) {
      lines(polygon_points$x[(nrow(polygon_points)-1):nrow(polygon_points)], polygon_points$y[(nrow(polygon_points)-1):nrow(polygon_points)], col = 'blue', lwd = 2)
    }
  }
  
  # Close the polygon by connecting the last point to the first
  polygon_points <- rbind(polygon_points, polygon_points[1,])
  
  # Create an sf object for the polygon
  polygon_sf <- st_polygon(list(as.matrix(polygon_points)))
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
        lines(c(points$x[2], left_intersection_points[i, 1]), c(points$y[2], left_intersection_points[i, 2]), col = 'blue', lwd = 2, lty = 2)
      }
    }
    
    if (nrow(right_intersection_points) > 0) {
      for (i in 1:nrow(right_intersection_points)) {
        lines(c(points$x[2], right_intersection_points[i, 1]), c(points$y[2], right_intersection_points[i, 2]), col = 'red', lwd = 2, lty = 2)
      }
    }
    
  } else {
    left_polygon <- NULL
    right_polygon <- NULL
    left_x_length <- 0
    right_x_length <- 0
  }
  
  # Calculate the area of the original, left, and right polygons
  total_area <- calculate_polygon_area(polygon_sf) / (scale_factor^2)
  left_area <- if (!is.null(left_polygon)) calculate_polygon_area(left_polygon) / (scale_factor^2) else 0
  right_area <- if (!is.null(right_polygon)) calculate_polygon_area(right_polygon) / (scale_factor^2) else 0
  
  # Create a data frame to store the area and length values
  results_df <- data.frame(
    Total_Area = as.numeric(total_area),
    Left_Area = as.numeric(left_area),
    Right_Area = as.numeric(right_area),
    Left_X_Length = as.numeric(left_x_length) / scale_factor,
    Right_X_Length = as.numeric(right_x_length) / scale_factor
  )
  
  # Create a list to store the spatial information
  spatial_info <- list(
    original_polygon = polygon_sf,
    left_polygon = left_polygon,
    right_polygon = right_polygon
  )
  
  # Return the data frame and the spatial information
  return(list(results_df = results_df, spatial_info = spatial_info))
}

# SET WORKING DIRECTORY
setwd("C:/Users/49157/Desktop/personal data/Uni/Master/Sommersemester 2024/Forest Entomology/data")

# Media and fungi lists
media_name <- c("SPAM", "YEMA")
media_short <- c("S", "Y")

fungi_list <- c("Grosmannia penicillata", "Beauveria bassiana", "Endoconidiophora", "Wickerhamomyces bisporus", "Rhodotorula sp")
fungi_list_short <- c("G", "B", "E", "W", "R")

# Initialize an empty data frame to store all samples
all_samples <- data.frame()

# Sample parameters
media <- media_short[1]
fungi_name_main <- fungi_list_short[1]
fungi_name_sub <- fungi_list_short[3]
number_sample <- 6

# Create a unique sample name
sample_name <- paste0(fungi_name_main, fungi_name_sub, media, number_sample)

# Path to your image
image_path <- "D:/Fungi photos/21_06_2024/DSC_0501.NEF"

# Call the function to get the polygon information
polygon_info <- select_points_and_draw_cartesian_and_polygon(image_path)

# Add the sample name to the results data frame
polygon_info$results_df$Sample_Name <- sample_name

# Reorder the columns to have Sample_Name first
polygon_info$results_df <- polygon_info$results_df[, c("Sample_Name", setdiff(names(polygon_info$results_df), "Sample_Name"))]

# Combine the results with the all_samples data frame
all_samples <- rbind(all_samples, polygon_info$results_df)

print(all_samples)

# Save the all_samples data frame to a CSV file
write.csv(all_samples, paste0("samples_from_", Sys.Date(), ".csv"), row.names = FALSE)

# Print the updated all_samples data frame
print("All Samples Data Frame:")
print(all_samples)

# Print the spatial information for the current sample
print("Spatial Information for Current Sample:")
print(polygon_info$spatial_info)
