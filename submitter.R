library(batchtools)
library(data.table)

# TODO
# ====

# This is actually no bad system!!!
# Procedere to run experiments on HPC with python and R batchtools:
# * Python script to generate table (setup.csv) of parameters (simple csv-file with header and
#   one line per experiment:
#   jobid param1 param2 ...
#.  NOTE: this works well for scalar arguments only. For complex parameters,
#.  one could simply store jobid paramfile where paramfile is the path to the
#   file which contains the (serialized) arguments dictionary.
# * generate a runner.py which takes the jobid as single command line argument
# * batchJobs::batchMap over all jobids: just call the runner, pass jobid and
#.  redirect output into jobid.out
# * Collect results in out.dir via R or command line; merge with setup.csv via
#.  unique ID (jobid)

source("study1/defs.R")
file.dir = "study1-benchmark"

if (!dir.exists(out.dir))
  dir.create(out.dir, recursive = TRUE)

unlink(file.dir, recursive = TRUE)
reg = batchtools::makeRegistry(file.dir = file.dir, source = "study1/defs.R")
#reg$cluster.functions = batchtools::makeClusterFunctionsMulticore(ncpus = parallel::detectCores())

runner = function(id) {
  out.file = file.path(out.dir, sprintf("%i.out", id))
  args = c(exp.runner.file, id, ">", out.file)
  args = BBmisc::collapse(args, sep = " ")
  system2(exp.command, args)
}

exp = read.table(exp.setup.file, header = TRUE, stringsAsFactors = FALSE)
n = nrow(exp)

chunk.size = 50L
ids = batchtools::batchMap(runner, id = seq_len(n))
ids = ids[, chunk := chunk(job.id, chunk.size = chunk.size)]

BBmisc::pause()

submitJobs(ids = ids, resources = list(walltime = 60 * 60 * 6, mem = 4000))
