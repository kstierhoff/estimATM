estimate.cps.nasc <- function(path.input, pattern, path.output, max.range = 200, transparency = 0.2,
                              expand.left = F, expand.right = F, expansion = 2, suffix = "_nasc_cps", 
                              jpeg = T, root = 1.5, scaling = 1) {
  exponent <- 1
  
  if (expand.left == T) {
    exponent <- 1 / expansion
  }
  
  if (expand.right == T) {
    exponent <- expansion
  }
  
  acoustic.file.list <- list.files(path = path.input, pattern = pattern, recursive = F)
  print(acoustic.file.list)
  
  print("Type the order of the file you want to integrate")
  test.1 <- scan(n = 1)
  
  for (i in acoustic.file.list[test.1]) {
    print(acoustic.file.list[test.1])
    
    processed.file <- list.files(
      path = path.input,
      pattern =
        paste(substr(acoustic.file.list[test.1], 1, nchar(acoustic.file.list[test.1]) - 4), "_nasc_cps", sep = ""),
      recursive = F
    )
    
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
        pattern =
          paste(substr(acoustic.file.list[test.1], 1, nchar(acoustic.file.list[test.1]) - 4), "_nasc_cps", sep = ""),
        recursive = F
      )
      
      if (length(processed.file) > 0) {
        print(paste("There is already a processed file with the name", processed.file))
      }
      print("Is this the file you want to integrate? 1 = Yes; 0 = No")
      test.2 <- scan(n = 1)
    }
  }
  ## so far it is working
  
  print(paste("Expansion =", exponent))
  
  temporary <- read.csv(paste(path.input, "\\", acoustic.file.list[test.1], sep = ""),
                        fileEncoding = "UTF-8-BOM"
  )
  
  # plot
  if (jpeg == T) {
    jpeg(paste(path.output, "\\", substr(acoustic.file.list[test.1], 1, nchar(acoustic.file.list[test.1]) - 4), "_original", ".jpeg", sep = ""), width = 35, height = 25, units = "cm", res = 200)
    
    plot(temporary$Interval^exponent, temporary$Depth_mean, ylim = c(max.range, 0), type = "n", main = acoustic.file.list[test.1], ylab = "Depth (m)", xlab = "Interval", xaxt = "n") # this is better because it follows the images
    axis(side = 1, at = seq(min(temporary$Interval^exponent), max(temporary$Interval^exponent), l = 5), lab = floor(seq(min(temporary$Interval^exponent), max(temporary$Interval^exponent), l = 5)^(1 / exponent)))
    
    
    lines(unlist(lapply(split(temporary$Interval^exponent, temporary$Interval), FUN = unique)), unlist(lapply(split(temporary$Depth_mean, temporary$Interval), FUN = max)), col = 1, lwd = 2)
    
    points(temporary$Interval^exponent, temporary$Depth_mean, cex = (temporary$NASC / (50000 / scaling))^(1 / root) * 30, pch = 19, col = rgb(t(col2rgb("blue")) / 255, alpha = transparency))
    points((temporary$Interval[temporary$NASC / 50000 * 30 > 1])^exponent, temporary$Depth_mean[temporary$NASC / 50000 * 30 > 1], pch = ".", cex = 2, col = "red")
    
    abline(h = 50, col = "gray", lwd = 0.5)
    abline(h = 100, col = "gray", lwd = 0.5)
    abline(h = 150, col = "gray", lwd = 0.5)
    abline(h = 200, col = "gray", lwd = 0.5)
    abline(h = 250, col = "gray", lwd = 0.5)
    abline(h = 300, col = "gray", lwd = 0.5)
    abline(h = 0, col = "gray", lwd = 0.5)
    dev.off()
  }
  
  
  x11()
  plot(temporary$Interval^exponent, temporary$Depth_mean, ylim = c(max.range, 0), type = "n", main = acoustic.file.list[test.1], ylab = "Depth (m)", xlab = "Interval", xaxt = "n") # this is better because it follows the images
  axis(side = 1, at = seq(min(temporary$Interval^exponent), max(temporary$Interval^exponent), l = 5), lab = floor(seq(min(temporary$Interval^exponent), max(temporary$Interval^exponent), l = 5)^(1 / exponent)))
  
  
  lines(unlist(lapply(split(temporary$Interval^exponent, temporary$Interval), FUN = unique)), unlist(lapply(split(temporary$Depth_mean, temporary$Interval), FUN = max)), col = 1, lwd = 2)
  
  points(temporary$Interval^exponent, temporary$Depth_mean, cex = (temporary$NASC / (50000 / scaling))^(1 / root) * 30, pch = 19, col = rgb(t(col2rgb("blue")) / 255, alpha = transparency))
  points((temporary$Interval[temporary$NASC / 50000 * 30 > 1])^exponent, temporary$Depth_mean[temporary$NASC / 50000 * 30 > 1], pch = ".", cex = 2, col = "red")
  
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
    b <- approx(new.lines.bottom$x, new.lines.bottom$y, xout = temporary$Interval^exponent, rule = 2, method = "linear")$y
    points(temporary$Interval^exponent, b, cex = 1, col = col.line, pch = 19)
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
      b <- approx(new.lines.top$x, new.lines.top$y, xout = temporary$Interval^exponent, rule = 2, method = "linear")$y
      points(temporary$Interval^exponent, b, cex = 1, col = col.line, pch = 19)
      col.line <- col.line + 1
      print("Does it look good now? 1 = Yes; 0 = No")
      test.3 <- scan(n = 1)
    }
    
    temporary$top.habitat <- b
  }
  
  if (test.4 == 0) {
    temporary$top.habitat <- 0
  }
  
  temporary$cps.nasc <- temporary$NASC
  temporary$cps.nasc[temporary$Depth_mean < temporary$top.habitat | temporary$Depth_mean > temporary$bottom.habitat] <- 0
  
  # plot
  x11()
  plot(temporary$Interval^exponent, temporary$Depth_mean, ylim = c(max.range, 0), type = "n", main = acoustic.file.list[test.1], ylab = "Depth (m)", xlab = "Interval", xaxt = "n")
  axis(side = 1, at = seq(min(temporary$Interval^exponent), max(temporary$Interval^exponent), l = 5), lab = floor(seq(min(temporary$Interval^exponent), max(temporary$Interval^exponent), l = 5)^(1 / exponent)))
  
  lines(unlist(lapply(split(temporary$Interval^exponent, temporary$Interval), FUN = unique)), unlist(lapply(split(temporary$Depth_mean, temporary$Interval), FUN = max)), col = 1, lwd = 2)
  
  points(temporary$Interval^exponent, temporary$Depth_mean, cex = (temporary$cps.nasc / (50000 / scaling))^(1 / root) * 30, pch = 19, col = rgb(t(col2rgb("blue")) / 255, alpha = transparency))
  points((temporary$Interval[temporary$cps.nasc / 50000 * 30 > 1])^exponent, temporary$Depth_mean[temporary$cps.nasc / 50000 * 30 > 1], pch = ".", cex = 2, col = "red")
  
  abline(h = 50, col = "gray", lwd = 0.5)
  abline(h = 100, col = "gray", lwd = 0.5)
  abline(h = 150, col = "gray", lwd = 0.5)
  abline(h = 200, col = "gray", lwd = 0.5)
  abline(h = 250, col = "gray", lwd = 0.5)
  abline(h = 300, col = "gray", lwd = 0.5)
  abline(h = 0, col = "gray", lwd = 0.5)
  
  
  if (test.4 == 1) {
    polygon(c(new.lines.top$x, rev(new.lines.bottom$x)), c(new.lines.top$y, rev(new.lines.bottom$y)), col = rgb(t(col2rgb("yellow")) / 255, alpha = 0.15), border = NA)
  }
  points(temporary$Interval^exponent, temporary$top.habitat, cex = 0.5, col = "red", pch = 19)
  points(temporary$Interval^exponent, temporary$bottom.habitat, cex = 0.5, col = "blue", pch = 19)
  
  if (jpeg == T) {
    jpeg(paste(path.output, "\\", substr(acoustic.file.list[test.1], 1, nchar(acoustic.file.list[test.1]) - 4), "_clean", ".jpeg", sep = ""), width = 35, height = 25, units = "cm", res = 200)
    plot(temporary$Interval^exponent, temporary$Depth_mean, ylim = c(max.range, 0), type = "n", main = acoustic.file.list[test.1], ylab = "Depth (m)", xlab = "Interval", xaxt = "n")
    axis(side = 1, at = seq(min(temporary$Interval^exponent), max(temporary$Interval^exponent), l = 5), lab = floor(seq(min(temporary$Interval^exponent), max(temporary$Interval^exponent), l = 5)^(1 / exponent)))
    
    lines(unlist(lapply(split(temporary$Interval^exponent, temporary$Interval), FUN = unique)), unlist(lapply(split(temporary$Depth_mean, temporary$Interval), FUN = max)), col = 1, lwd = 2)
    
    points(temporary$Interval^exponent, temporary$Depth_mean, cex = (temporary$cps.nasc / (50000 / scaling))^(1 / root) * 30, pch = 19, col = rgb(t(col2rgb("blue")) / 255, alpha = transparency))
    points((temporary$Interval[temporary$cps.nasc / 50000 * 30 > 1])^exponent, temporary$Depth_mean[temporary$cps.nasc / 50000 * 30 > 1], pch = ".", cex = 2, col = "red")
    
    abline(h = 50, col = "gray", lwd = 0.5)
    abline(h = 100, col = "gray", lwd = 0.5)
    abline(h = 150, col = "gray", lwd = 0.5)
    abline(h = 200, col = "gray", lwd = 0.5)
    abline(h = 250, col = "gray", lwd = 0.5)
    abline(h = 300, col = "gray", lwd = 0.5)
    abline(h = 0, col = "gray", lwd = 0.5)
    
    if (test.4 == 1) {
      polygon(c(new.lines.top$x, rev(new.lines.bottom$x)), c(new.lines.top$y, rev(new.lines.bottom$y)), col = rgb(t(col2rgb("yellow")) / 255, alpha = 0.15), border = NA)
    }
    points(temporary$Interval^exponent, temporary$top.habitat, cex = 0.5, col = "red", pch = 19)
    points(temporary$Interval^exponent, temporary$bottom.habitat, cex = 0.5, col = "blue", pch = 19)
    dev.off()
  }
  
  write.csv(temporary,
            paste0(path.output, "\\", substr(acoustic.file.list[test.1], 1, nchar(acoustic.file.list[test.1]) - 4), suffix, ".csv"),
            row.names = FALSE,
            quote = FALSE
  )
}
