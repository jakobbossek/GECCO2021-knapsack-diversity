library(tidyverse)

source("study1/defs.R")

fs = list.files(out.dir.local, pattern = ".out$", full.names = TRUE)
BBmisc::catf("Processing %i files ...", length(fs))

tbl = sapply(fs, function(f) {
  tmp = readLines(f)
  jobid = gsub(".out", "", basename(f))
  return(paste(jobid, tmp[length(tmp)], sep = " "))
})

mergedfile = file.path("study1/data", "final.csv")
tbl = c("jobid iters time_passed entropy iters_without_improvement repair_operations repair_success", tbl)
#writeLines(tbl, mergedfile)

# merge
setup = readr::read_delim(exp.setup.file, delim = " ")
result = readr::read_delim(mergedfile, delim = " ")
merged = dplyr::left_join(setup, result, by = "jobid")
merged$y.jobid = NULL
write.table(merged, file = mergedfile, row.names = FALSE, quote = TRUE)
