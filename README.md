# DownScale
A collection of downscaling scripts and input datasets for creating high-resolution GLOBIOM output.

1. [Setup](#-setup) 
2. [Dependencies](#-dependencies)
3. [Running DownScale](#-running-downscale)
4. [Which script is for me?](#-choosing-the-right-script)

# Setup
Clone the repository to a working directory able to hold up to a gigabyte. For example somewhere on your H: drive. If you use the Git command line client use

      git clone <repository URL>

You can obtain the repository URL by clicking on the 'Code' drop-down menu at the top right of the GitHub repository page.

# Dependencies

The prior module is dependent on R, particularly the BayesLogit package.

The downscaling module itself requries a GAMS installation, with a valid NLS solver license (for 1_downscaling.gms and 1_downscalingEconometric.gms) and a CONOPT4 license for 1_downscalingEconometricMNL.gms.

# Running DownScale

The R prior estimation framework is in the prior_module subfolder. The code comes with estimated priors though, so you do not need to run this.

You have to provide a GLOBIOM output file output in input folder

The files should contain (at the least): LUC_COMPARE_SCEN0, PRICE_COMPARE2

Either the file 1_downscaling.gms (for using only non-estimated priors) or 1_downscalingEconometric.gms (for using the estimated priors also) can be run.

Change in respective script at line 37 which GLOBIOM file to load.

# Choosing the right script

The econometric priors (1_downscalingEconometric.gms) are the current best practice to run (as they produce the closest to relaistic maps).

- 1_downscaling.gms relies entirely on non-estimated modeller defined priors and uses a maximum entropy downscaler.
- 1_downscalingEconometric.gms uses ESA-CCI land-use change data in 2000-2010 to calibrate prior dynamics between cropland, grassland, forest and other natural vegetation and uses a maximum entropy downscaler.
- 1_downscalingEconometricMNL.gms relies on above calibrated priors, but uses a squared differences downscaler, with a multinomial model specification (it is the fastest).

For guidance see the documentation here: https://bit.ly/3fiLG3u
