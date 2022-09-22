estimate.cps.nasc <- function(path.input, pattern, path.output, path.img = NULL, pattern.img = NULL, max.range = 200, 
                              transparency = 0.2, expand.left = FALSE, expand.right = FALSE, expansion = 2, 
                              suffix, jpeg, root = 1.5, scaling = 1) {
  exponent <- 1
  
  if (expand.left) {
    exponent <- 1 / expansion
  }
  
  if (expand.right) {
    exponent <- expansion
  }
  
  # List CSV files for processing
  acoustic.file.list <- list.files(path = path.input, pattern = pattern, recursive = F)
  print(acoustic.file.list)
  
  print("Type the order of the file you want to integrate:")
  test.1 <- scan(n = 1)
  
  for (i in acoustic.file.list[test.1]) {
    print(acoustic.file.list[test.1])
    
    processed.file <- list.files(
      path = path.input,
      pattern = paste0(substr(acoustic.file.list[test.1], 1, nchar(acoustic.file.list[test.1]) - 4), "_nasc_cps"),
      recursive = FALSE)
    
    if (length(processed.file) > 0) {
      print(paste("There is already a processed file with the name", processed.file))
    }
    
    
    print("Is this the file you want to integrate? 1 = Yes; 0 = No")
    test.2 <- scan(n = 1)
    
    while (test.2 == 0) {
      print(acoustic.file.list)
      print("Chose another file")
      test.1 <- scan(n = 1)
      print(acoustic.file.list[test.1])
      processed.file <- list.files(
        path = path.input,
        pattern = paste0(substr(acoustic.file.list[test.1], 1, nchar(acoustic.file.list[test.1]) - 4), "_nasc_cps"),
        recursive = FALSE)
      
      if (length(processed.file) > 0) {
        print(paste("There is already a processed file with the name", processed.file))
      }
      print("Is this the file you want to integrate? 1 = Yes; 0 = No")
      test.2 <- scan(n = 1)
    }
  }
  # Read the CSV file
  temporary <- read.csv(paste0(path.input, "\\", acoustic.file.list[test.1]),
                        fileEncoding = "UTF-8-BOM")
  
  if (!is.null(path.img)) {
    # List the exported images
    img.file.list <- list.files(path = path.img, pattern = pattern.img, recursive = F)
    print(img.file.list)
    
    print("Type the order of the exported echogram to open:")
    img.1 <- scan(n = 1)
    
    # Open the selected image
    shell.exec(file.path(path.img, img.file.list[img.1]))
  }
  
  # plot
  if (jpeg) {
    jpeg(paste0(path.output, "\\", substr(acoustic.file.list[test.1], 1, nchar(acoustic.file.list[test.1]) - 4), "_original.jpeg"), 
         width = 35, height = 25, units = "cm", res = 200)
    
    plot(temporary$Dist_M^exponent, 
         temporary$Depth_mean, 
         ylim = c(max.range, 0), type = "n", 
         main = acoustic.file.list[test.1], 
         ylab = "Depth (m)", xlab = "Dist_M", xaxt = "n") # this is better because it follows the images
    axis(side = 1, at = seq(min(temporary$Dist_M^exponent), 
                            max(temporary$Dist_M^exponent), l = 5), 
         lab = floor(seq(min(temporary$Dist_M^exponent), 
                         max(temporary$Dist_M^exponent), l = 5)^(1 / exponent)))
    
    
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
  
  
  x11(width = 1200, height = 400)
  plot(temporary$Dist_M^exponent, 
       temporary$Depth_mean, 
       ylim = c(max.range, 0), type = "n", 
       main = acoustic.file.list[test.1], 
       ylab = "Depth (m)", xlab = "Dist_M", 
       xaxt = "n") # this is better because it follows the images
  axis(side = 1, at = seq(min(temporary$Dist_M^exponent), 
                          max(temporary$Dist_M^exponent), l = 5), 
       lab = floor(seq(min(temporary$Dist_M^exponent), 
                       max(temporary$Dist_M^exponent), l = 5)^(1 / exponent)))
  
  
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
  
  abline(h = 50, col = "gray", lwd = 0.5)
  abline(h = 100, col = "gray", lwd = 0.5)
  abline(h = 150, col = "gray", lwd = 0.5)
  abline(h = 200, col = "gray", lwd = 0.5)
  abline(h = 250, col = "gray", lwd = 0.5)
  abline(h = 300, col = "gray", lwd = 0.5)
  abline(h = 0, col = "gray", lwd = 0.5)
  
  # Manual drawing only
  test.3 <- 0
  col.line <- 3
  
  while (test.3 == 0) {
    print("Select the bottom integration range by clicking on the desired locations - stop by right-clicking")
    new.lines.bottom <- locator(type = "l", lty = 2, lwd = 0.5)
    b <- approx(new.lines.bottom$x, 
                new.lines.bottom$y, 
                xout = temporary$Dist_M^exponent, rule = 2, method = "linear")$y
    points(temporary$Dist_M^exponent, b, cex = 1, col = col.line, pch = 19)
    col.line <- col.line + 1
    print("Does it look good now? 1 = Yes; 0 = No")
    test.3 <- scan(n = 1)
  }
  
  temporary$bottom.habitat <- b
  
  # top habitat
  
  print("Do you want to fix the top layer? 1 = Yes; 0 = No")
  test.4 <- scan(n = 1)
  
  if (test.4 == 1) {
    test.3 <- 0
    col.line <- 4
    while (test.3 == 0) {
      print("Select the top integration range by clicking on the desired locations - stop by right-clicking")
      new.lines.top <- locator(type = "l", lty = 2, lwd = 0.5)
      b <- approx(new.lines.top$x, new.lines.top$y, xout = temporary$Dist_M^exponent, rule = 2, method = "linear")$y
      points(temporary$Dist_M^exponent, b, cex = 1, col = col.line, pch = 19)
      col.line <- col.line + 1
      print("Does it look good now? 1 = Yes; 0 = No")
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
  x11(width = 1200, height = 400)
  plot(temporary$Dist_M^exponent, 
       temporary$Depth_mean, 
       ylim = c(max.range, 0), type = "n", 
       main = acoustic.file.list[test.1], ylab = "Depth (m)", xlab = "Dist_M", xaxt = "n")
  axis(side = 1, at = seq(min(temporary$Dist_M^exponent), 
                          max(temporary$Dist_M^exponent), l = 5), 
       lab = floor(seq(min(temporary$Dist_M^exponent), 
                       max(temporary$Dist_M^exponent), l = 5)^(1 / exponent)))
  
  lines(unlist(lapply(split(temporary$Dist_M^exponent, temporary$Dist_M), FUN = unique)), 
        unlist(lapply(split(temporary$Depth_mean, temporary$Dist_M), FUN = max)), col = 1, lwd = 2)
  
  points(temporary$Dist_M^exponent, 
         temporary$Depth_mean, 
         cex = (temporary$cps.nasc / (50000 / scaling))^(1 / root) * 30, 
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
         temporary$top.habitat, cex = 0.5, col = "red", pch = 19)
  points(temporary$Dist_M^exponent, 
         temporary$bottom.habitat, cex = 0.5, col = "blue", pch = 19)
  
  if (jpeg) {
    jpeg(paste0(path.output, "\\", substr(acoustic.file.list[test.1], 1, nchar(acoustic.file.list[test.1]) - 4), "_clean.jpeg"), 
         width = 35, height = 25, units = "cm", res = 200)
    plot(temporary$Dist_M^exponent, 
         temporary$Depth_mean, 
         ylim = c(max.range, 0), type = "n", main = acoustic.file.list[test.1], 
         ylab = "Depth (m)", xlab = "Dist_M", xaxt = "n")
    axis(side = 1, at = seq(min(temporary$Dist_M^exponent), 
                            max(temporary$Dist_M^exponent), l = 5), 
         lab = floor(seq(min(temporary$Dist_M^exponent), 
                         max(temporary$Dist_M^exponent), l = 5)^(1 / exponent)))
    
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
            paste0(path.output, "\\", substr(acoustic.file.list[test.1], 1, nchar(acoustic.file.list[test.1]) - 4), suffix, ".csv"),
            row.names = FALSE,
            quote = FALSE
  )
  
  # Close both graphics devices
  graphics.off()
}
