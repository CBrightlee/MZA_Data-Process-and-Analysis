## Step 1: Initial Flux Processing
Begin by running your downloaded Ameriflux file through the Initial Flux Processing script to remove spikes. This can also be used to view the dataset across a time series and to see diurnal trends or patterns in the data. It will generate an output with the spikes removed for later use if desired.
Stop here or proceed to Steps 2 & 3

## Step 2: Run this code through the EC-FW-Hotspot-Mapping code
There is a separate repository with the MatLab code necessary to run this hotspot footprint analysis. The output from Step 1 should be put into that. Follow the ReadMe and directions there to complete the run. 

## Step 3: Analyze the matrix outputs
Once you have your matrix outputs from Step 2, they can be run through the FinalStats_Project1 code. Everything should plug in easily, but search for all setwd areas and change them to your desired file path. 
