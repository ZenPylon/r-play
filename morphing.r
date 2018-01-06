#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)

#number of frames or plots
frames <- 50

# function for creating file name with leading zeros
# makes it easier to process them sequentially
rename <- function(x){
  if (x < 10) {
    return(name <- paste('000',i,'plot.png',sep=''))p
  }
  if (x < 100 && i >= 10) {
    return(name <- paste('00',i,'plot.png', sep=''))
  }
  if (x >= 100) {
    return(name <- paste('0', i,'plot.png', sep=''))
  }
}

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
  name <- rename(i);
  
  #saves the plot as a .png file in the working directory
  png(name)
  print(i)
  newFunction <- createHomotopy(i/frames);
  curve(newFunction, from=0, to=5, xlim=c(0, 6), ylim=c(0, 30));
  dev.off()
}

#run ImageMagick
my_command <- 'ffmpeg -pattern_type glob -i \'*.png\' -c:v libx264 -r 30 -pix_fmt yuv420p out.mp4';
system(my_command)