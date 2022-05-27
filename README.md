# Irish_electricity

* ```individual_GAM_forEachCust_HPC.R``` is an R script that create a GAM for each customer and iterates week by week to make a prediction for the next week. These scripts use log(dem + 0.001) as the response variable, in order to avoid trying to take log(0) and getting errors. 
* ```individual_GAM_forEachCust_HPC_parallel.R``` does this, but parallelises making a GAM for each household at each weeks iteration to speed up. 
* There are corresponding ```.sh``` files for both of these scripts.
* ```IndGAM_log_allPredz_parallel_3dp.txt``` outputs the predictions to 3 d.p. from the above script, where the repsonse was logged before being fed to the model. This isn't on the github page as it is over the 100MB threshold permitted. 
* ```Data_setup.R``` is a short file for reading in the ```RData``` file: **Irish.RData** and seeing the 3 different tables contained within this dataset.
* ```DynamicModel.R``` is a file that is is incomplete. Trying to make a model that takes in only the last 3 days of data to forecast for the upcoming day. It is incomplete. 
* ```commonGAM_HPC.R``` is a file for running the GAM that models all the households aggregated together to a common model. It uses the HPC and some parallelisation to speed up it's running. There is a `.sh` file that goes with this.


* ```HPC_jobs.ods``` is a spreadsheet that records all the jobs sent off to the HPC during this project. 
