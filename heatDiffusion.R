library('animation')

#Time parameters
simulation.timestep = .02
simulation.totalDuration = 10
simulation.numTimesteps = simulation.totalDuration / simulation.timestep

#cells per side

simulation.plot.numCells = 50
simulation.plot.xValues = 1:simulation.plot.numCells
simulation.plot.yValues = 1:simulation.plot.numCells

#Heat data
heatMatrix <- matrix( rnorm(simulation.plot.numCells ^ 2), 
                      simulation.plot.numCells, 
                      simulation.plot.numCells )

#weight of surrounding heat values versus the center heat value
transferRatio <- .13
colorMap = heat.colors(50)

runSimulation <- function() {
  for (i in seq(1, simulation.numTimesteps)) {
    print(paste("heatMatrix", heatMatrix[1, 1]))
    
    image(simulation.plot.xValues, simulation.plot.yValues, heatMatrix, col = colorMap);
    updateHeatMatrix()
  }
}

updateHeatMatrix <- function() {
    updatedMatrix <- matrix(0, simulation.plot.numCells, simulation.plot.numCells);
    for (i in seq(1, simulation.plot.numCells)) {
      for (j in seq(1, simulation.plot.numCells)) {
      #  print(paste("i and j", i, j))
        
        centerHeat <- heatMatrix[i, j]
        
        #Collect the surrounding values if they are within bounds of the matrix
        xRange <- max(j - 1, 1) : min(j + 1, simulation.plot.numCells)
        yRange <- max(i - 1, 1) : min(i + 1, simulation.plot.numCells)
        surroundingHeat <- mean(heatMatrix[xRange, yRange])
        
        updatedMatrix[i, j] = transferRatio * surroundingHeat + (1 - transferRatio) * centerHeat
        
        #print(paste("old value", heatMatrix[i, j]))
        #print(paste("new value", updatedMatrix[i, j]))
      }
    }
    print(paste("updatedMatrix", updatedMatrix[1, 1]))
    heatMatrix <<- updatedMatrix
}

opts = ani.options(ani.width = 1200, ani.height = 800, other.opts = "-pix_fmt yuv420p -b 600k")
saveVideo(runSimulation(), video.name = "heatWave.mp4", interval = simulation.timestep, ani.options = opts)
ani.options(opts)
