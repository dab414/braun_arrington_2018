Data and scripts for Braun & Arrington's "Assessing the role of reward in task selection using a reward-based voluntary task switching procedure"
	Published in Psychological Research, September 2017

I'll preface this by saying that I'm mostly self taught in R, and I'm still learning. There are some techniques that I learned by the end of this anaylsis (e.g., data table) that I didn't know at the beginning, and the scripts reflect this type of progression. I tried to make everything as clear as I could in spite of this, and all data manipulations and analyses are (to the best of my knowledge) statistically sound. If anything is unclear, please feel free to contact me with questions:

My email: dab414@lehigh.edu
	
General layout

	./scripts 
		contains all R scripts categorized by analysis stage
			e.g., preprocessing, plotting, running statistical models
			
	./results
		all scripts are programmed to save output to the corresponding directory in the "results" folder, with few exceptions
			e.g., the "./scripts/lrt_checks/lrt_run.r" saves the output of the analysis to "./results/lrt_check/file_name.rdata"

	./data
		this directory includes: 
			"easy_feed_raw.txt" -- the raw data
			"var_code.txt" -- an explanation of all the final variables included in the data (after preprocessing)
			
		the "./scripts/preprocessing/preprocessing.r" script takes as input the raw data and outputs several different datasets to "./data"
			after running the preprocessing script, you should see the following datasets in "./data/"
				"easy_clean.csv" -- the final, preprocessed dataset used for the main analyses
				"easy_clean_errors.csv" -- same as above except subjects are trimmed according to +2SD above the mean error rate, as opposed to trimming according to a constant 15% error rate
					This dataset is generated from the "./scripts/preprocessing/preprocessing_errors.r" script
				"easy_clean_full.csv" -- no trimming on the point difference variable, point difference is still range -10:10
				"easy_clean_rt.csv" -- trimmed trials more extreme than +/- 2SDs from subject means, this dataset was used for analyzing performance data
				"run_length.csv" -- a dataset used for analyzing individual differences
				"easy_feed_raw.txt" -- the raw data (this file will be here BEFORE running 'preprocessing.r', see note above)
				
				
A general method for recreating the analyses:

	First run "./scripts/preprocessing/preprocessing.r"
		Outputs the relevant, cleaned datasets
		
	Then run "./scripts/lrt_checks/lrt_run.r"
		Runs and outputs results of the main glmer analyses
		
	For running the current task value X other task value ANOVA, run "./scripts/anova/anova_selection.r"
	
	For running followups on interactions from glmer, use "./scripts/int_followup"
	
	For generating figures, see "./scripts/figures/"
	
	and so on
	
I tried to heavily comment each of the scripts so that it's clear exactly what's going on.
I'm assuming the required libraries are installed; if you run into an error with this, just run:
	install.packages('desired_package_name')
	
Please feel free to ask me any questions → dab414@lehigh.edu

Best,
Dave Braun






			