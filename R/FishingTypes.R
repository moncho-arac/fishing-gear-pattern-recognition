library(trajr)
library(ggfortify)

traj_Statistics <-function(traj)
{
  # Statistics for trajectories
  
  #Calculates the step angles (in radians) of each segment,
  traj_angles      <-TrajAngles(traj)
  traj_angles_mean <-mean(traj_angles)
  traj_angles_std  <-sd(traj_angles)
  traj_angles_min  <-min(traj_angles)
  traj_angles_max  <-max(traj_angles)
  
  # Speed
  derivs <- TrajDerivatives(traj)
  speed_mean <-mean(derivs$speed)
  speed_std  <-sd(derivs$speed)
  speed_min  <-min(derivs$speed)
  speed_max  <-max(derivs$speed)
  
  # Sinousity
  sinousity    <-TrajSinuosity2(traj)
  straightness <-TrajStraightness(traj)
  emax         <-TrajEmax(traj)
  
  #Distance
  traj_distance  <-TrajDistance(traj)
  traj_expec_dis <-TrajExpectedSquareDisplacement(traj)
  traj_length    <-TrajLength(traj)
  
  # Directional change
  directional_change       <-TrajDirectionalChange(traj)
  directional_change_mean  <-mean(directional_change)
  directional_change_std   <-sd(directional_change)
  directional_change_min   <-min(directional_change)
  directional_change_max   <-max(directional_change)
  
  list(
    traj_angles_mean = traj_angles_mean,
    traj_angles_std  = traj_angles_std,
    traj_angles_min  = traj_angles_min,
    traj_angles_max  = traj_angles_max,
    
    speed_mean  = speed_mean,
    speed_std   = speed_std,
    speed_min   = speed_min,
    speed_max   = speed_max,
    
    directional_change_mean = directional_change_mean,
    directional_change_std  = directional_change_std,
    directional_change_min  = directional_change_min,
    directional_change_max  = directional_change_max,
    
    traj_distance  = traj_distance,
    traj_expec_dis = traj_expec_dis,
    traj_length    = traj_length,
    
    sinousity     = sinousity,
    straightness  = straightness,
    emax          = emax
  )
}

#===============================================================================
traj_histogram2D<-function(traj)
{
  
  angles     <- TrajAngles(traj)
  derivs     <- TrajDerivatives(traj)
  speed      <- derivs$speed[1:length(angles)]
  
  h2d<-hist2d(angles, speed, nbins=50, show=FALSE)
  mh2d<-unname(as.matrix(h2d$counts))
    
  return (c(mh2d))
  
}
#===============================================================================

#Load the csv
files<-list("Trawlers_good.txt", "Purse_seines_good.txt", "Drifting_longlines_good.txt") # Files names
types <-list("Trawl","Seines", "Longline") # Types should by the same "order" as filenames

trjs <-list()                   # List for all trajectories
f_type <- list()                # Saving types for classification

index_type<-1

for (fileName in files)
{
  conn <- file(fileName,open="r")
  linn <-readLines(conn)
  
  type = types[[index_type]]
  
  # Load all the CSV by their name in fileName
  for (i in 1:length(linn))
  {
    print(paste(type, "Read trayectorie:",linn[i]))
    # Load the csv files
    coords <- read.csv(linn[i], header=FALSE)
    # Manually assign the header names
    names(coords) <- c("Id",'lon', 'lat', 'dat', 'time')

    coords[,'time'] <- TrajConvertTime(coords[,'time'], factors = c(60*60,60,1))

    trj <- TrajFromCoords(coords,xCol='lon',yCol='lat',timeCol='time')

    trjs[[length(trjs) + 1]] <- trj # Add corrected trajectory

    f_type[[length(f_type) + 1]] <- type # Add type
  }

  index_type<-index_type+1
  close(conn)
}

#================================= STADISTICS ================================
table <-TrajsMergeStats(trjs,traj_Statistics) # Statistics

# Create the data frames
c_df <- data.frame(matrix(unlist(f_type), nrow=length(f_type), byrow=T),stringsAsFactors=FALSE)
names(c_df) <- c("FT")
table       <- cbind(table,c_df)
table       <- na.omit(table)

#Write to file
write.csv(table,"FishingTypesStats.csv", row.names = FALSE)

#================================= HISTOGRAMS ==================================
table <-TrajsMergeStats(trjs,traj_histogram2D)

# Combine the data frames
c_df <- data.frame(matrix(unlist(f_type), nrow=length(f_type), byrow=T),stringsAsFactors=FALSE)
names(c_df) <- c("FT")
table       <- cbind(table,c_df)
table       <- na.omit(table)

write.csv(table,"FishingTypesHist.csv", row.names = FALSE)
#===============================================================================
