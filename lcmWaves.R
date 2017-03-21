library('animation')

#Time parameters
simulation.timestep = .02
simulation.totalDuration =
simulation.frequencyScalar = 20
simulation.numTimesteps = simulation.totalDuration / simulation.timestep

#cells per side

simulation.plot.numCells = 100
simulation.plot.xValues = 1:simulation.plot.numCells
simulation.plot.yValues = 1:simulation.plot.numCells

#Heat data
waveMatrix <- matrix( 0, 
                      simulation.plot.numCells, 
                      simulation.plot.numCells )

colorMap = rainbow(50)

runSimulation <- function() {
  for (i in seq(1, simulation.numTimesteps)) {
    print('timestep')
    image(simulation.plot.xValues, simulation.plot.yValues, waveMatrix, col = colorMap);
    updateWaveMatrix(i)
  }
}

updateWaveMatrix <- function(n) {
  for (i in seq(1, simulation.plot.numCells)) {
    for (j in seq(1, simulation.plot.numCells)) {
      waveMatrix[i, j] <<- sin(2*pi * (i/simulation.frequencyScalar) * n*simulation.timestep) + 
                         sin(2*pi * (j/simulation.frequencyScalar) * n*simulation.timestep)
    }
  }
}

opts = ani.options(ani.width = 1200, ani.height = 800, other.opts = "-pix_fmt yuv420p -b 600k")
saveVideo(runSimulation(), video.name = "lcmWaves.mp4", interval = simulation.timestep, ani.options = opts)
ani.options(opts)
