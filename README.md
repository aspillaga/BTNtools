# BTNtools

**BTNtools** is a package developed by the **Balearic Tracking Network** for 
the analysis of acoustic telemetry data, primarily focused on aquatic species 
tracking. The package provides a set of tools for processing, analyzing, and 
visualizing detection data from acoustic telemetry studies.

## Functions

### Detection management:
- **detectFilter**: Identifies potential false detections in acoustic telemetry 
                    data based on the intervals between consecutive detections.
- **mainVal**: Identifies and returns the most frequent value in a vector.
               Useful to identify the receiver with the largest number of 
               detections within a time interval.

### Trajectory analysis:
- **timeChunks**: Divides time stamps into chunks based on gaps exceeding a 
                  specified threshold.
- **chunkSeq**: Generates regular time sequences for time chunks.
- **trackFilt**: Filters movement trajectories by removing unrealistic locations 
                 based on speed thresholds and trajectory spikes. 
- **stepLength**: Calculates step lengths (distance in meters) between 
                  consecutive positions in a movement trajectory.
- **speedCalc**: Calculates the speed (m/s) between consecutive positions in a 
                 movement trajectory.
- **bearing**: Calculates the bearing (movement direction) between consecutive 
               positions in a movement trajectory.
- **turnAng**: Calculates the turning angle (change in movement direction) 
               between consecutive steps in a trajectory.


### Activity estimation:
- **milliSec**: Extracts the millisecond component from POSIXct timestamps.
- **soundSpeed**: Calculates the speed of sound in water using the Mackenzie 
                  equation.
- **unwrapDrift**: Unwraps time drift in detection data.

### Plot functions:
- **rescale**: Rescales numeric values to a specified range.
- **plotScale**: Adds a scale bar to a map with UTM projection.
- **axisLonLat**: Draws axes with latitude and longitude labels on a UTM map.


## Installation

You can install the package directly from GitHub using `devtools` or `remotes`:

```R
# Install devtools if you haven't already
install.packages("devtools")

# Install BTNtools from GitHub
devtools::install_github("aspillaga/BTNtools")
```

## Contact
For further inquiries or feedback, feel free to contact me at 
aspillaga@imedea.uib-csic.es

