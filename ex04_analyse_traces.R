library(tidyverse)
library(ggplot2)
library(scales)

# IMPORT DATA AND TIDY UP
# ===

dir_data = "study1/data"
dir_data_raw = file.path(dir_data, "raw")
dir_images = "study1/images"

evals = readr::read_delim(file.path(dir_data, "final.csv"), delim = " ")
evals = filter(evals, mutator != "bbf", mu %in% c(25, 50, 100), eps != 0.2, generator == "uncorr", crossover == "off", repair == "deterministic")
rename = c("abf" = "EDO-BBF1", "sbf" = "EDO-BBF2", "bf" = "BF", "pbf" = "PBF", "htbf" = "HTBF")

# make algorithms factors
evals$mutator = sapply(evals$mutator, function(x) rename[x])
evals$mutator = factor(evals$mutator, levels = c("BF", "PBF", "HTBF", "EDO-BBF1", "EDO-BBF2"), ordered = TRUE)
evals$facet_var = sprintf("(%s, %i, %.2f, %i)", evals$generator, evals$D, evals$eps, evals$mu)

load_traces = function(tbl, step = 100, iters_definitive = c()) {
  res = lapply(seq_len(nrow(tbl)), function(i) {
    worktbl = tbl[i, ]
    fn = file.path(dir_data_raw, sprintf("%i.out", worktbl$jobid))
    #print(fn)
    trace = read.table(
      file = fn,
      header = FALSE,
      nrow = length(readLines(fn)) - 1L)
    colnames(trace) = c("iter", "diversity", "improved", "infeasible", "repaired")
    worktbl = cbind(trace, worktbl)
    filter(worktbl, ((iter %% step) == 0) | iter == 1 | iter %in% iters_definitive)
  })
  return(dplyr::as_tibble(do.call(rbind, res)))
}

traces = filter(evals, repl == 1, D %in% c(2, 5, 10), eps %in% c(0.5), mu == 100, generator %in% c("scorr", "uncorr")) %>%
  load_traces(step = 25L, iters_definitive = seq(1, 100, by = 5))

g = ggplot(traces, aes(x = iter, y = diversity, shape = mutator, color = mutator, group = mutator))
g = g + geom_path(alpha = 0.5) + geom_point(size = 0.8)
g = g + geom_vline(xintercept = 100, linetype = "dashed", color = "gray")
g = g + scale_color_brewer(palette = "Dark2")
g = g + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
g = g + theme_bw()
g = g + theme(legend.position = "top", legend.margin = margin(0,0,0,0), legend.box.margin = margin(-8,-8,-8,-8))
g = g + guides(colour = guide_legend(nrow = 1))
g = g + facet_wrap(. ~ facet_var, ncol = 3)
g = g + labs(
  #title = "Aggregated Trajectories",
  #subtitle = sprintf("Repair mechanism: %s", REPAIR),
  y = "Population diversity / entropy",
  x = "Iteration (log-scaled)",
  color = "Mutation operator",
  shape = "Mutation operator")
#print(g)
fn = file.path(dir_images, "traces.pdf")
ggsave(fn, plot = g, width = 7, height = 3.1)
