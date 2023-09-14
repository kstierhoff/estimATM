extractNASC <- function(path.in, pattern.in, path.out, suffix.out, 
                        x11.w = 1600, x11.h = 600, jpeg = TRUE,
                        path.img = NULL, pattern.img = NULL, 
                        dist.bin = 2000,
                        max.range = 350, transparency = 0.2, 
                        root = 1.5, scaling = 0.1, expansion = 2) {
  
  # List CSV files for processing
  acoustic.file.list <- list.files(path = path.in, pattern = pattern.in, recursive = F)
  print(acoustic.file.list)
  
  cat("Type the order of the file you want to integrate:\n")
  test.1 <- scan(n = 1)
  
  for (i in acoustic.file.list[test.1]) {
    print(acoustic.file.list[test.1])
    
    processed.file <- list.files(
      path = path.in,
      pattern = paste0(substr(acoustic.file.list[test.1], 1, nchar(acoustic.file.list[test.1]) - 4), suffix.out),
      recursive = FALSE)
    
    if (length(processed.file) > 0) {
      cat(paste("There is already a processed file with the name", processed.file,".\n"))
    }
    
    cat("Is this the file you want to integrate? 1 = Yes; 0 = No\n")
    test.2 <- scan(n = 1)
    
    while (test.2 == 0) {
      print(acoustic.file.list)
      cat("Chose another file:")
      test.1 <- scan(n = 1)
      print(acoustic.file.list[test.1])
      processed.file <- list.files(
        path = path.in,
        pattern = paste0(substr(acoustic.file.list[test.1], 1, nchar(acoustic.file.list[test.1]) - 4), suffix.out),
        recursive = FALSE)
      
      if (length(processed.file) > 0) {
        cat(paste("There is already a processed file with the name", processed.file, ".\n"))
      }
      cat("Is this the file you want to integrate? 1 = Yes; 0 = No\n")
      test.2 <- scan(n = 1)
    }
  }
  
  # Get file name to process
  acoustic.file.name <- acoustic.file.list[test.1]
  # Extract prefix to 
  acoustic.file.prefix <- unlist(strsplit(acoustic.file.name, "-"))[1]
  # Create name for output file
  acoustic.file.out <- paste0(unlist(strsplit(acoustic.file.name, "[.]"))[1], suffix.out)
  
  # Read the CSV file
  temporary <- read.csv(file.path(path.in, acoustic.file.name),
                        fileEncoding = "UTF-8-BOM")
  
  if (!is.null(path.img)) {
    # If the image path is specified, open the corresponding echogram image
    echogram.img <- file.path(path.img, 
                              paste0(gsub(pattern.in, "", acoustic.file.name), pattern.img))
    
    if (file.exists(echogram.img)) {
      shell.exec(echogram.img)
    } else {
      cat("No echogram named:", echogram.img, ".\n\n")
    }
  }
  
  # Initialize the expansion exponent for the x-axis
  exponent <- 1
  
  # plot
  if (jpeg) {
    jpeg(paste0(path.out, "\\", substr(acoustic.file.list[test.1], 1, nchar(acoustic.file.list[test.1]) - 4), "_original.jpeg"), 
         width = 35, height = 25, units = "cm", res = 200)
    
    plot(temporary$Dist_M^exponent, 
         temporary$Depth_mean, 
         ylim = c(max.range, 0), type = "n", 
         main = acoustic.file.list[test.1], 
         ylab = "Depth (m)", xlab = "Dist_M", xaxt = "n") # this is better because it follows the images
    axis(side = 1, at = seq(min(temporary$Dist_M^exponent), 
                            max(temporary$Dist_M^exponent), l = 10), 
         lab = floor(seq(min(temporary$Dist_M^exponent), 
                         max(temporary$Dist_M^exponent), l = 10)^(1 / exponent)))
    
    lines(unlist(lapply(split(temporary$Dist_M^exponent, temporary$Dist_M), FUN = unique)), 
          unlist(lapply(split(temporary$Depth_mean, temporary$Dist_M), FUN = max)), col = 1, lwd = 2)
    
    points(temporary$Dist_M^exponent, 
           temporary$Depth_mean, 
           cex = (temporary$NASC / (50000 / scaling))^(1 / root) * 30, 
           pch = 19, col = rgb(t(col2rgb("blue")) / 255, alpha = transparency))
    points((temporary$Dist_M[temporary$NASC / 50000 * 30 > 1])^exponent, 
           temporary$Depth_mean[temporary$NASC / 50000 * 30 > 1], 
           pch = ".", cex = 2, col = "red")
    
    abline(h = 50,  col = "gray", lwd = 0.5)
    abline(h = 100, col = "gray", lwd = 0.5)
    abline(h = 150, col = "gray", lwd = 0.5)
    abline(h = 200, col = "gray", lwd = 0.5)
    abline(h = 250, col = "gray", lwd = 0.5)
    abline(h = 300, col = "gray", lwd = 0.5)
    abline(h = 0,   col = "gray", lwd = 0.5)
    graphics.off()
  }
  
  # Create interactive plot to pick top and bottom habitat lines
  x11(width = x11.w, height = x11.h)
  plot(temporary$Dist_M^exponent, 
       temporary$Depth_mean, 
       ylim = c(max.range, 0), type = "n", 
       main = acoustic.file.list[test.1], 
       ylab = "Depth (m)", xlab = "Dist_M", 
       xaxt = "n") # this is better because it follows the images
  axis(side = 1, at = seq(min(temporary$Dist_M^exponent), 
                          max(temporary$Dist_M^exponent), l = 10), 
       lab = floor(seq(min(temporary$Dist_M^exponent), 
                       max(temporary$Dist_M^exponent), l = 10)^(1 / exponent)))
  
  lines(unlist(lapply(split(temporary$Dist_M^exponent, temporary$Dist_M), FUN = unique)), 
        unlist(lapply(split(temporary$Depth_mean, temporary$Dist_M), FUN = max)), 
        col = 1, lwd = 2)
  
  points(temporary$Dist_M^exponent, 
         temporary$Depth_mean, 
         cex = (temporary$NASC / (50000 / scaling))^(1 / root) * 30, 
         pch = 19, 
         col = rgb(t(col2rgb("blue")) / 255, alpha = transparency))
  
  points((temporary$Dist_M[temporary$NASC / 50000 * 30 > 1])^exponent, 
         temporary$Depth_mean[temporary$NASC / 50000 * 30 > 1], pch = ".", cex = 2, col = "red")
  
  abline(h =  50, col = "gray", lwd = 0.5)
  abline(h = 100, col = "gray", lwd = 0.5)
  abline(h = 150, col = "gray", lwd = 0.5)
  abline(h = 200, col = "gray", lwd = 0.5)
  abline(h = 250, col = "gray", lwd = 0.5)
  abline(h = 300, col = "gray", lwd = 0.5)
  abline(h =   0, col = "gray", lwd = 0.5)
  
  # Ask to expand left or right axes
  cat("Do you want to expand the left or right axis? 1 = Yes; 0 = No\n")
  axis.1 <- scan(n = 1)
  
  if (axis.1 == 1) {
    cat("Which axis do you want to expand? 1 = Left; 2 = Right\n")
    axis.2 <- scan(n = 1)
    
    # Expand left axis
    if (axis.2 == 1) {
      cat("Expanding left axis.\n")
      exponent <- 1 / expansion
    }
    
    if (axis.2 == 2) {
      cat("Expanding right axis.\n")
      exponent <- expansion
    }
    
    # Close active device
    dev.off()
    
    # Redraw interactive plot to pick top and bottom habitat lines
    x11(width = x11.w, height = x11.h)
    plot(temporary$Dist_M^exponent, 
         temporary$Depth_mean, 
         ylim = c(max.range, 0), type = "n", 
         main = acoustic.file.list[test.1], 
         ylab = "Depth (m)", xlab = "Dist_M", 
         xaxt = "n") # this is better because it follows the images
    axis(side = 1, at = seq(min(temporary$Dist_M^exponent), 
                            max(temporary$Dist_M^exponent), l = 10), 
         lab = floor(seq(min(temporary$Dist_M^exponent), 
                         max(temporary$Dist_M^exponent), l = 10)^(1 / exponent)))
    
    lines(unlist(lapply(split(temporary$Dist_M^exponent, temporary$Dist_M), FUN = unique)), 
          unlist(lapply(split(temporary$Depth_mean, temporary$Dist_M), FUN = max)), 
          col = 1, lwd = 2)
    
    points(temporary$Dist_M^exponent, 
           temporary$Depth_mean, 
           cex = (temporary$NASC / (50000 / scaling))^(1 / root) * 30, 
           pch = 19, 
           col = rgb(t(col2rgb("blue")) / 255, alpha = transparency))
    
    points((temporary$Dist_M[temporary$NASC / 50000 * 30 > 1])^exponent, 
           temporary$Depth_mean[temporary$NASC / 50000 * 30 > 1], pch = ".", cex = 2, col = "red")
    
    abline(h =  50, col = "gray", lwd = 0.5)
    abline(h = 100, col = "gray", lwd = 0.5)
    abline(h = 150, col = "gray", lwd = 0.5)
    abline(h = 200, col = "gray", lwd = 0.5)
    abline(h = 250, col = "gray", lwd = 0.5)
    abline(h = 300, col = "gray", lwd = 0.5)
    abline(h =   0, col = "gray", lwd = 0.5)
    
  } else {
    cat("No axes are being expanded.\n")
  }
  
  # Manual drawing only
  test.3 <- 0
  col.line <- 3
  
  while (test.3 == 0) {
    cat("Select the bottom integration range by clicking on the desired locations - stop by right-clicking.\n")
    new.lines.bottom <- locator(type = "l", lty = 2, lwd = 0.5)
    b <- approx(new.lines.bottom$x, 
                new.lines.bottom$y, 
                xout = temporary$Dist_M^exponent, rule = 2, method = "linear")$y
    points(temporary$Dist_M^exponent, b, cex = 1, col = col.line, pch = 19)
    col.line <- col.line + 1
    cat("Does it look good now? 1 = Yes; 0 = No\n")
    test.3 <- scan(n = 1)
  }
  
  temporary$bottom.habitat <- b
  
  # top habitat
  cat("Do you want to fix the top layer? 1 = Yes; 0 = No\n")
  test.4 <- scan(n = 1)
  
  if (test.4 == 1) {
    test.3 <- 0
    col.line <- 4
    while (test.3 == 0) {
      cat("Select the top integration range by clicking on the desired locations - stop by right-clicking.\n")
      new.lines.top <- locator(type = "l", lty = 2, lwd = 0.5)
      b <- approx(new.lines.top$x, new.lines.top$y, xout = temporary$Dist_M^exponent, rule = 2, method = "linear")$y
      points(temporary$Dist_M^exponent, b, cex = 1, col = col.line, pch = 19)
      col.line <- col.line + 1
      cat("Does it look good now? 1 = Yes; 0 = No\n")
      test.3 <- scan(n = 1)
    }
    
    temporary$top.habitat <- b
  }
  
  if (test.4 == 0) {
    temporary$top.habitat <- 0
  }
  
  # Define CPS nasc
  temporary$cps.nasc <- temporary$NASC
  temporary$cps.nasc[temporary$Depth_mean < temporary$top.habitat | temporary$Depth_mean > temporary$bottom.habitat] <- 0
  
  # plot
  x11(width = x11.w, height = x11.h)
  plot(temporary$Dist_M^exponent, 
       temporary$Depth_mean, 
       ylim = c(max.range, 0), type = "n", 
       main = acoustic.file.list[test.1], ylab = "Depth (m)", xlab = "Dist_M", xaxt = "n")
  axis(side = 1, at = seq(min(temporary$Dist_M^exponent), 
                          max(temporary$Dist_M^exponent), l = 10), 
       lab = floor(seq(min(temporary$Dist_M^exponent), 
                       max(temporary$Dist_M^exponent), l = 10)^(1 / exponent)))
  
  lines(unlist(lapply(split(temporary$Dist_M^exponent, temporary$Dist_M), FUN = unique)), 
        unlist(lapply(split(temporary$Depth_mean, temporary$Dist_M), FUN = max)), col = 1, lwd = 2)
  
  points(temporary$Dist_M^exponent, 
         temporary$Depth_mean, 
         cex = (temporary$cps.nasc / (50000 / scaling))^(1 / root) * 30, 
         pch = 19, col = rgb(t(col2rgb("blue")) / 255, alpha = transparency))
  points((temporary$Dist_M[temporary$cps.nasc / 50000 * 30 > 1])^exponent, 
         temporary$Depth_mean[temporary$cps.nasc / 50000 * 30 > 1], 
         pch = ".", cex = 2, col = "red")
  
  abline(h =  50, col = "gray", lwd = 0.5)
  abline(h = 100, col = "gray", lwd = 0.5)
  abline(h = 150, col = "gray", lwd = 0.5)
  abline(h = 200, col = "gray", lwd = 0.5)
  abline(h = 250, col = "gray", lwd = 0.5)
  abline(h = 300, col = "gray", lwd = 0.5)
  abline(h =   0, col = "gray", lwd = 0.5)
  
  if (test.4 == 1) {
    polygon(c(new.lines.top$x, rev(new.lines.bottom$x)), 
            c(new.lines.top$y, rev(new.lines.bottom$y)), 
            col = rgb(t(col2rgb("yellow")) / 255, alpha = 0.15), border = NA)
  }
  points(temporary$Dist_M^exponent, 
         temporary$top.habitat, cex = 0.5, col = "red", pch = 19)
  points(temporary$Dist_M^exponent, 
         temporary$bottom.habitat, cex = 0.5, col = "blue", pch = 19)
  
  if (jpeg) {
    jpeg(paste0(path.out, "\\", substr(acoustic.file.list[test.1], 1, nchar(acoustic.file.list[test.1]) - 4), "_clean.jpeg"), 
         width = 35, height = 25, units = "cm", res = 200)
    plot(temporary$Dist_M^exponent, 
         temporary$Depth_mean, 
         ylim = c(max.range, 0), type = "n", main = acoustic.file.list[test.1], 
         ylab = "Depth (m)", xlab = "Dist_M", xaxt = "n")
    axis(side = 1, at = seq(min(temporary$Dist_M^exponent), 
                            max(temporary$Dist_M^exponent), l = 10), 
         lab = floor(seq(min(temporary$Dist_M^exponent), 
                         max(temporary$Dist_M^exponent), l = 10)^(1 / exponent)))
    
    lines(unlist(lapply(split(temporary$Dist_M^exponent, temporary$Dist_M), FUN = unique)), 
          unlist(lapply(split(temporary$Depth_mean, temporary$Dist_M), FUN = max)), col = 1, lwd = 2)
    
    points(temporary$Dist_M^exponent, 
           temporary$Depth_mean, cex = (temporary$cps.nasc / (50000 / scaling))^(1 / root) * 30, 
           pch = 19, col = rgb(t(col2rgb("blue")) / 255, alpha = transparency))
    points((temporary$Dist_M[temporary$cps.nasc / 50000 * 30 > 1])^exponent, 
           temporary$Depth_mean[temporary$cps.nasc / 50000 * 30 > 1], 
           pch = ".", cex = 2, col = "red")
    
    abline(h = 50,  col = "gray", lwd = 0.5)
    abline(h = 100, col = "gray", lwd = 0.5)
    abline(h = 150, col = "gray", lwd = 0.5)
    abline(h = 200, col = "gray", lwd = 0.5)
    abline(h = 250, col = "gray", lwd = 0.5)
    abline(h = 300, col = "gray", lwd = 0.5)
    abline(h = 0,   col = "gray", lwd = 0.5)
    
    if (test.4 == 1) {
      polygon(c(new.lines.top$x, rev(new.lines.bottom$x)), 
              c(new.lines.top$y, rev(new.lines.bottom$y)), 
              col = rgb(t(col2rgb("yellow")) / 255, alpha = 0.15), border = NA)
    }
    points(temporary$Dist_M^exponent, 
           temporary$top.habitat, 
           cex = 0.5, col = "red", pch = 19)
    points(temporary$Dist_M^exponent, 
           temporary$bottom.habitat, 
           cex = 0.5, col = "blue", pch = 19)
    graphics.off()
  }
  
  write.csv(temporary,
            file.path(path.out, paste0(unlist(strsplit(acoustic.file.name, "[.]"))[1], suffix.out)),
            row.names = FALSE,
            quote = FALSE
  )
  
  # Close both graphics devices
  graphics.off()
  
  # Create final figure showing results of processing ---------------------------
  # Read new nasc_cps.csv file
  new.masked.file <- read.csv(
    file.path(path.out,
              paste0(unlist(strsplit(acoustic.file.name, "[.]"))[1], 
                     suffix.out))) %>% 
    arrange(NASC)
  
  # Create GPS status data frame; missing Lat/Lon (999) will plot red
  gps.status <- new.masked.file %>% 
    select(Dist_M, Lat_M, Lon_M) %>% 
    group_by(Dist_M) %>% 
    summarize(Lat_M = Lat_M[1],
              Lon_M = Lon_M[1]) %>% 
    mutate(gps.good = case_when(
      is.na(Lat_M) | is.na(Lon_M) ~ FALSE,
      TRUE ~ TRUE)) %>% 
    arrange(Dist_M)

  ## Summarize file for plotting the seabed depth
  seabed.depth <- new.masked.file %>% 
    arrange(Dist_M) %>% 
    group_by(Dist_M) %>% 
    summarize(max.depth = -max(Depth_mean))
  
  ## Plot results
  cps.nasc.bubble <- ggplot(new.masked.file) +
    # Plot the seabed
    geom_line(data = seabed.depth,
              aes(Dist_M, max.depth), alpha = 0.5) +
    # Plot the surface
    geom_hline(yintercept = 1) +
    # Plot the top habitat line
    geom_line(aes(Dist_M, -top.habitat), colour = "red", linetype = "dashed") +
    # Plot the bottom habitat line
    geom_line(aes(Dist_M, -bottom.habitat), colour = "blue", linetype = "dashed") +
    # Plot NASC that was removed
    geom_point(data = filter(new.masked.file, NASC != cps.nasc),
               aes(Dist_M, -Depth_mean, size = NASC),
               shape = 21, fill = NA, alpha = 0.8, show.legend = FALSE) +
    # Plot NASC that was retained
    geom_point(data = filter(new.masked.file, NASC > 0, NASC == cps.nasc),
               aes(Dist_M, -Depth_mean, size = NASC, fill = NASC),
               shape = 21, alpha = 0.9, show.legend = FALSE) +
    # Plot GPS status
    geom_point(data = gps.status, aes(Dist_M, 5, colour = gps.good), size = 1) +
    # Configure axes and scales
    scale_x_continuous(position = "top", breaks = seq(0, max(new.masked.file$Dist_M), dist.bin), expand = c(0,0)) +
    scale_y_continuous(breaks = -rev(seq(0, signif(max(new.masked.file$Depth_mean), 1), 50))) +
    scale_size_area(breaks = c(0,100,1000,10000,50000,100000,1000000),
                    guide = guide_legend(reverse = TRUE)) +
    scale_fill_viridis_c(option = "plasma") +
    scale_colour_manual(name = "GPS Good", values = c("TRUE" = "green", "FALSE" = "red")) +
    # Configure labels and title
    labs(title = acoustic.file.out,
         x = "Echoview distance (M)", 
         y = "Mean depth (m)") + 
    # Set themes
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust = 1.2),
          legend.position = c(0.95,0.1),
          legend.justification = c(1,0.1)) 
  
  # Save the plot
  ggsave(cps.nasc.bubble,
         filename = file.path(path.out, 
                              paste0(unlist(strsplit(acoustic.file.name, "[.]"))[1], 
                                     gsub(".csv",".png", suffix.out))),
         width = 15, height = 7)
  
  # Read echogram image and create ggplot2 object
  ev.image <- magick::image_read(echogram.img) %>% magick::image_ggplot()
  
  # Combine bubble plot and echogram image
  bubble.ev.combo <- cps.nasc.bubble / ev.image + 
     plot_layout(heights = c(1, 1)) 
  
  # Save bubble plot and echogram image
  ggsave(bubble.ev.combo,
         filename = file.path(path.out, 
                              paste0(unlist(strsplit(acoustic.file.name, "[.]"))[1], "_combo",
                                     gsub(".csv",".png", suffix.out))),
         width = 15, height = 7)
  
  # Open the newly created plots
  ## Open bubble plot
  shell.exec(file.path(path.out, 
                       paste0(unlist(strsplit(acoustic.file.name, "[.]"))[1], 
                              gsub(".csv",".png", suffix.out))))
  
  # Open bubble plot and echogram image combo
  shell.exec(file.path(path.out, 
                       paste0(unlist(strsplit(acoustic.file.name, "[.]"))[1], "_combo",
                              gsub(".csv",".png", suffix.out))))
  
  # Print message at the end of processing
  cat("Finished processing file:", acoustic.file.name, 
      "\n\nFor complaints and/or feature requests, contact J. Zwolinski (juan.zwolinski@noaa.gov) ;)\n")
}
