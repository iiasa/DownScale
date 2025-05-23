# Use paths relative to the working directory, with / as path separator.
EXPERIMENT = "DownScaling" # label for your run, pick something short but descriptive without spaces and valid as part of a filename
PREFIX = "_condor" # prefix for per-job .err, log, .lst, and .out files
JOBS = c(1:120)
HOST_REGEXP = "^limpopo" # a regular expression to select execute hosts from the cluster
REQUEST_MEMORY = 7800 # memory (MiB) to reserve for each job
REQUEST_CPUS = 1 # number of hardware threads to reserve for each job
LAUNCHER = "Rscript" # interpreter with which to launch the script
SCRIPT = "cluster_code_prior_module.R" # script that comprises your job
ARGUMENTS = "%1" # arguments to the script
BUNDLE_INCLUDE_DIRS = c("input") # recursive, supports wildcards
BUNDLE_EXCLUDE_DIRS = c("Condor", "output") # recursive, supports wildcards
BUNDLE_EXCLUDE_FILES = c("*.log") # supports wildcards
BUNDLE_ADDITIONAL_FILES = c() # additional files to add to root of bundle, can also use an absolute path for these
RETAIN_BUNDLE = FALSE
GET_OUTPUT = TRUE
OUTPUT_DIR = "output" # relative to working dir both host-side and on the submit machine
OUTPUT_FILE = "output.RData" # as produced by a job on the execute-host, will be remapped with EXPERIMENT and cluster/job numbers to avoid name collisions when transferring back to the submit machine.
WAIT_FOR_RUN_COMPLETION = TRUE
CONDOR_DIR = "Condor" # directory where Condor reference files are stored in a per-experiment subdirectory (.err, .log, .out, .job and so on files)
SEED_JOB_RELEASES = 4 # number of times to auto-release held seed jobs before giving up
JOB_RELEASES = 5 # number of times to auto-release held jobs before giving up
