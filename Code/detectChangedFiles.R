library(fs)
# Take initial snapshot
fs1 <- fileSnapshot("E:/CODE/R_packages/estimATM/2107RL/Figs",full.names = TRUE)
# Take new snapshot
fs2 <- fileSnapshot("E:/CODE/R_packages/estimATM/2107RL/Figs",full.names = TRUE)

# Changed files
changedFiles(fs1, fs2)

# Get list of changed file names
fs::path_file(changedFiles(fs1, fs2)$deleted)
