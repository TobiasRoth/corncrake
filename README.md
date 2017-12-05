# Analyses of Corncrake calling site occupancy at the Lower Oder Valley National Park in northeastern Germany

The repository contains JAGS code for the occupancy model that allows for staggered arrival and departure times . The occupancy model estimates within-season movements of male Corncrakes and analyses how departure probabilities, calling site occupancy is affected by land use. 

The model is based on [Kendall, W. L., Hines, J. E., Nichols, J. D., & Grant, E. H. C. (2013). Relaxing the closure assumption in occupancy models: staggered arrival and departure times. *Ecology*, 94(3), 610-617.](http://onlinelibrary.wiley.com/doi/10.1890/12-1720.1/full)

The following paper is based on the presented model: [Arbeiter, S., Roth, T., Helmecke, A., Haferland, H. J., & Bellebaum, J. How to count a vagabond?–Population estimation in the Corncrake Crex crex.](https://www.researchgate.net/profile/Susanne_Arbeiter/publication/314286417_How_to_count_a_vagabond_-_Population_estimation_in_the_Corncrake_Crex_crex/links/58c13feaaca2720944010ef5/How-to-count-a-vagabond-Population-estimation-in-the-Corncrake-Crex-crex.pdf)

The repository contains the following documents

## Scripts
### Run_MCMC.R: R-Script to perform the MCMC analyses

With this script 
- the data are loaded from the `RData` folder, 
- prepared to use in the MCMC-analyses, 
- the JAGS analyses are launched and 
- the results from the MCMC analyses are saved in the folder `MCMCresults`


### Jagsmod.R: Description of the MODEL in BUGS language
This files describes the hierarchical site-occupancy model we used to analyze the pine marten data. The model describes the occurrence probability of pine martens per 1-km^2 using the following variables (in parentheses the names of the parameters are given as used in the BUGS-model):
- Intercept (a0)
- percentage of forest within 1-km^2  (a1)
- area of old forest (stem diameter > 50 cm) within all forest in 1-km^2 (a2)
- total length of larger roads within 1-km^2 (a3)

The probability to record  a pine marten at a camera-trap location (given pine marten is occurring at the respective 1-km^2 square) is described using the following variables:
- Random Effekt (b0[i]) to describe unexplained variation between camera-trap locations (*i*)
- Julian day included as linear (b1) and quadratic effect (b2) to allow for a peak in detection probability during the study period.


### Compile_results.R: Script to compile the MCMC results as presented in the Paper
This R-Script loads the MCMC results to digest for the presentation in the manuscript.

## Data

### bugsdat.RData
adf
