library(tidyverse)
library(ggplot2)
library(scales)

# IMPORT DATA AND TIDY UP
# ===

dir_data = "study1/data"
dir_data_raw = file.path(dir_data, "raw")

evals = readr::read_delim(file.path(dir_data, "final.csv"), delim = " ")
evals = filter(evals, mutator != "bbf", mu %in% c(25, 50, 100), eps != 0.2, generator != "invscorr")#crossover == "off",
rename = c("abf" = "EDO-BBF1", "sbf" = "EDO-BBF2", "bf" = "BF", "pbf" = "PBF", "htbf" = "HTBF")

# make algorithms factors
evals$mutator = sapply(evals$mutator, function(x) rename[x])
evals$mutator = factor(evals$mutator, levels = c("BF", "PBF", "HTBF", "EDO-BBF1", "EDO-BBF2"), ordered = TRUE)

peak_into_trace = function(job.id, rows) {
  fn = file.path(dir_data_raw, paste0(job.id, ".out"))
  lines = readLines(fn)[rows]
  as.numeric(sapply(strsplit(lines, split = " "), function(line) line[2L]))
}

evals$entropy_mu = sapply(1:nrow(evals), function(i) {
  job.id = as.integer(evals[i, "jobid"])
  mu = as.integer(evals[i, "mu"])
  peak_into_trace(job.id, rows = c(mu + 1))
})
#write.table(evals, file = file.path(dir_data, "evals.csv"), row.names = FALSE)

# Now calcualate for each experiment the maximum entropy achieved in all runs of all algorithms
evals_max = evals %>%
  group_by(generator, D, mu, eps) %>%
  dplyr::summarise(entropy_max = max(entropy))

evals2 = evals

# add max entropy
evals = dplyr::left_join(evals, evals_max, by = c("generator", "D", "mu", "eps"))
evals = filter(evals, entropy_max != 0)

# now normalize
evals = mutate(evals, entropy_dist = (1 - (entropy / entropy_max)) * 100, entropy_mu_dist = (1 - (entropy_mu / entropy_max)) * 100)

# to long format
evals = select(evals, generator, mutator, crossover, repair, D, mu, eps, entropy_dist, entropy_mu_dist)
evals_long = reshape2::melt(evals, id.vars = c("generator", "mutator", "D", "mu", "eps", "crossover"), variable.name = "time", value.name = "entropy") %>%
  mutate(time = recode_factor(time, `entropy_dist` = "mu * n " , `entropy_mu_dist` = "mu"))

g = ggplot(evals, aes(x = interaction(crossover, mutator, sep = " / "), y = entropy_mu_dist))
g = g + geom_boxplot(aes(color = repair))
g = g + theme_bw()
#g = g + scale_color_manual()
g = g + scale_color_brewer(labels = scales::parse_format(), palette = "Dark2")
g = g + theme(legend.position = "top", axis.text.x = element_text(hjust = 1, angle = 25))
g = g + scale_y_continuous()#scales::percent())
g = g + facet_grid(. ~ generator + mu)
g = g + labs(x = "Mutation operator", y = "Deviation from max. entropy [in %]", color = "Repair")
ggsave("study1/images/boxplots_anytime_dist_to_max_entropy_playground.pdf", width = 14, height = 4)
#print(g)


# Ranking of mutation operators
# ===
mutator_ranks = evals_long %>%
  group_by(generator, mutator, D, mu, eps, time) %>%
  dplyr::summarize(entropy = median(entropy)) %>%
  group_by(generator, D, mu, eps, time) %>%
  dplyr::mutate(rank = rank(entropy, ties.method = "random")) %>%
  ungroup()

table(mutator_ranks$rank, mutator_ranks$mutator, mutator_ranks$time) / 72 * 100
