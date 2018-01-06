library(rstudioapi) # load it
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#number of frames or plots
frames <- 100

startFunction <- function(x) {
  return(x*x);
}

endFunction <- function(x) {
  return(0);
}

createLinearHomotopy <- function(t) {
  newFunction <- function(x) { return ((1-t)*startFunction(x) + t*endFunction(x)) };
  return(newFunction);
}

createComplexHomotopy <- function(t) {
  newFunction <- function(x) { 
    return ( (1-(2*((1.6*t-1)^3-(.72*t-1))))*startFunction(x) + t*endFunction(x) );
  };
  return(newFunction);
}

if (!dir.exists('plots')) {
  dir.create('plots');
}

if (!dir.exists('plots/quadratic-linear-homotopy')) {
  dir.create('plots/quadratic-linear-homotopy');
}
if (!dir.exists('plots/quadratic-complex-homotopy')) {
  dir.create('plots/quadratic-complex-homotopy');
}

#loop through plots
for(i in 0:frames){

  #saves the plot as a .png file in the working directory
  png(paste('plots/quadratic-linear-homotopy/plot-', i, '.png', sep=''));
  print(i)
  newFunction <- createLinearHomotopy(i/(frames+1));
  curve(newFunction, from=0, to=5, xlim=c(0, 6), ylim=c(0, 30));
  dev.off()
}

#run ImageMagick
my_command <- 'ffmpeg -y -i plots/quadratic-linear-homotopy/plot-%d.png -c:v libx264 -r 30 -vf "setpts=1.3*PTS" -pix_fmt yuv420p quadratic-linear-homotopy.mp4';
system(my_command)


#loop through plots
for(i in 0:frames){
  
  #saves the plot as a .png file in the working directory
  png(paste('plots/quadratic-complex-homotopy/plot-', i, '.png', sep=''));
  print(i)
  newFunction <- createComplexHomotopy(i/(frames+1));
  curve(newFunction, from=0, to=5, xlim=c(0, 6), ylim=c(-30, 30));
  dev.off()
}

#run ImageMagick
my_command <- 'ffmpeg -y -i plots/quadratic-complex-homotopy/plot-%d.png -c:v libx264 -r 30 -vf "setpts=1.3*PTS" -pix_fmt yuv420p quadratic-complex-homotopy.mp4';
system(my_command)

