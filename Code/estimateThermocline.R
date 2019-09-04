# Load CTD data
ctd <- read.csv("C:\\Users\\r\\Desktop\\CTD Analysis\\1706RL_UCTD_001_processed.asc", header = TRUE, sep = "")

# Parse out temperature and depth
depth <- -ctd$DepSM
temperature <- ctd$Tnc90C

# Keep data below the surface layer
temperature <- temperature[depth < -10]
depth <- depth[depth < -10]

# Keep data above deep layer
temperature <- temperature[depth > -80]
depth <- depth[depth > -80]

# Plot data
plot(temperature, depth)

# Fit data to equation and solve for coefficients using nonlinear least squares
# Tu  = Temperature at top of thermocline
# Tb  = Temperature at bottom of thermocline
# D   = Depth of middle of the thermocline
# W   = Width of the thermocline
model <- nls(temperature ~ Tu + (Tb-Tu)/(1+exp((depth-D)/(2*W))), 
         start=list(Tu = 15, Tb = 9, D = -30, W = 10),
         control=nls.control(maxiter=500, minFactor=1e-6, warnOnly = TRUE))

new.data <- data.frame(depth = seq(min(depth),max(depth),len = 100))
lines(predict(model, newdata = new.data), new.data$depth)
points(predict(model, newdata = data.frame(depth = coef(model)[3])), coef(model)[3], col = "red", pch = 21, cex = 1.5, bg = "red")
lines(c(coef(model)[3],coef(model)[3]), c(-40, -20))