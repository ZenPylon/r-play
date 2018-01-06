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

createHomotopy <- function(t) {
  newFunction <- function(x) { return ((1-t)*startFunction(x) + t*endFunction(x)) };
  return(newFunction);
}

#loop through plots
for(i in 1:frames){

  #saves the plot as a .png file in the working directory
  png(paste('plot-', i, '.png', sep=''));
  print(i)
  newFunction <- createHomotopy(i/frames);
  curve(newFunction, from=0, to=5, xlim=c(0, 6), ylim=c(0, 30));
  dev.off()
}

#run ImageMagick
my_command <- 'ffmpeg -y -i plot-%d.png -c:v libx264 -r 30 -vf "setpts=1.3*PTS" -pix_fmt yuv420p out.mp4';
system(my_command)
