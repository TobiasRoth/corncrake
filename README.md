# Analyses of Corncrake calling site occupancy at the Lower Oder Valley National Park in northeastern Germany

The repository contains BUGS code for the occupancy model that allows for staggered arrival and departure times. The occupancy model estimates within-season movements of male Corncrakes and analyses how departure probabilities and calling site occupancy is affected by land use. 

The model is based on [Kendall, W. L., Hines, J. E., Nichols, J. D., & Grant, E. H. C. (2013). Relaxing the closure assumption in occupancy models: staggered arrival and departure times. *Ecology*, 94(3), 610-617.](http://onlinelibrary.wiley.com/doi/10.1890/12-1720.1/full)

The following papers are based on the presented model: 

- [Arbeiter, S., Roth, T., Helmecke, A., Haferland, H. J., & Bellebaum, J. (2017). How to count a vagabond? – Population estimation in the Corncrake Crex. *Vogelwelt*, 137: 75-79. crex.](https://www.researchgate.net/profile/Susanne_Arbeiter/publication/314286417_How_to_count_a_vagabond_-_Population_estimation_in_the_Corncrake_Crex_crex/links/58c13feaaca2720944010ef5/How-to-count-a-vagabond-Population-estimation-in-the-Corncrake-Crex-crex.pdf)

- Arbeiter, S., Roth, T., Helmecke, A., Haferland, H. J., Tanneberger, F. & Bellebaum, J. (in prep.). Contrary land use effects on Corncrake Crex crex protection and habitat suitability in managed floodplain meadows. 

The repository contains the following scripts.

### Run_model.R: R-Script to perform the MCMC analyses

With this script 
- the data are loaded from the `RData` folder, 
- the JAGS analyses are launched and 
- the results from the MCMC analyses are saved in the folder `Results`


### Model.R: Description of the MODEL in BUGS language
This files describes the multi-state occupancy model to analyze Corncrake calling site occupancies. The model describes the observations (0: not observed, 1: observed) of calling corncrakes at the i=1,..., N calling sites, j=1,...,J visits over the t=1,...,nyears breeding seasons. The model contains the following parameters:

(1) Calling site occupancy (psi)
- *mu.a0[1]*:	average calling site occupancy during the first study period (1998-2000).		
- *mu.a0[1]*:	average calling site occupancy during the second study period (2012-2015).		
- *sd.a0*:	Standard deviation of calling site occupancies between calling sites.		
- *a1*:	Effect of the height (cm) of a calling site above the average flood level on calling site occupancy.
- *a2*:	Effect of the number of years without land-use on calling site occupancy	

(2) Arrival probability
- *beta[1:J, t]*:	Average arrival probability of corncrakes between the J visits and separately for each breeding season t. By definition, the beta in a given sum to one.	Fig. 1a	Fig. 1a

(3) Departure probability
- *mu.alpha0[j]*:	Average departure probability from visit j-1 to j.		
- *sd.alpha0[j]*:	Standard deviation of yearly differences in average departure probabilities from visit j-1 to j.
- *alpha1*:	Effect of management (0: no management during the 10-day period; 1: management during the 10-day period) on departure probability.	

(4) Detection probability
- *mu.p*:	Average detection probability.		
- *sd.p*:	Standard deviation of differences in detection probability between sites, visits and years.		
- *p1*:	Linear effect of the visit number on detection probability.		
- *p2*:	Quadratic effect of the visit number on detection probability.		


### Make_Figures.R: Script to compile the MCMC results and make some figures
This R-Script loads the MCMC results to digest for the presentation in the manuscript.


