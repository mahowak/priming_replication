# README for REPLICATING A FUNDAMENTAL FINDING INPSYCHOLINGUISTICS:SYNTACTIC PRIMING

# Exp1

This directory contains the following for Experiment 1, the dative written sentence completion priming manipulation that seeks to replicate Pickering & Branigan (1998).

- exp1_items.csv: These are the items that will be run for Experiment 1.
- exp1_power.R: Run the power analysis for Experiment 1, output exp1_power_analysis.csv.
- exp1_power_analysis.csv: The results of the Experiment 1 power analysis, output by exp1_power.R    
- plot_dative_wsc_from_meta_analysis.R: This creates the forest plots in the paper, which show estimates of effect sizes with confidence intervals from previous relevant papers for Experiment 1.
- analyze_exp1_power_results.R: Reads in the output of the power analysis and plots Bayes Factors and computes some summary stats on the power analysis.
- analyze_pilot_results.Rmd and analyze_pilot_results.html: R notebook for analyzing the pilot data, which is in pilot1_20201230. These same analyses will be run for the final version of the experiment.

# Exp2

Analagous to Exp1, this contains the following for Exp2:

- exp2_items.csv: These are the items that will be run for Experiment 2.
- exp2_power.R: Run the power analysis for Experiment 2, output exp2_power_analysis.csv.
- exp2_power_analysis.csv: The results of the Experiment 2 power analysis, output by exp1_power.R    
- plot_RC_wsc_from_meta.R: This creates the forest plots in the paper, which show estimates of effect sizes with confidence intervals from previous relevant papers for Experiment 2.
- analyze_exp2_power_results.R: Reads in the output of the power analysis and plots Bayes Factors and computes some summary stats on the power analysis.
- analyze_pilot_results.Rmd and analyze_pilot_results.html: R notebook for analyzing the pilot data, which is in pilot1_20201230. These same analyses will be run for the final version of the experiment.

# data

- master_spreadsheet_processed.csv: Meta-analysis data from Mahowald et al. (2016), which is used for obtaining estimated effect sizes for earlier studies.
- Corley_Scheepers: Data from Scheepers for earlier study. Not available here.

# pngs
  
- Figures in paper, output by various analysis scripts.

# pilot_20201230

This contains data from the 20201230 pilot study, with 8 participants.

coded_results_exp1.csv and coded_results_exp2.csv contain the coded results for each experiment. 

The exp1_items.csv and exp2_items.csv folders contain the items run in the pilot, which may differ from the final items run (which appear in the folders exp1 and exp2).

# convert_ibex_to_csv.R

This takes the raw Ibex files and converts them to a csv for coding. We then copy to a Google Doc and perform blind coding, and save the output as coded csvs.
