library(tidyverse)
library(ggplot2)

source("study1/defs.R")

image_dir = "study1/images"

tbl = readr::read_delim("study1/data/final.csv", delim = " ")
print(tbl)

tbl = tbl %>%
  arrange(generator, D, eps, mu)
tbl$facet_var = sprintf("(%s, %i, %.2f, %i)", tbl$generator, tbl$D, tbl$eps, tbl$mu)

# THE BIG PICTURE
# ===
for (REPAIR in unique(tbl$repair)) {
  g = ggplot(filter(tbl, repair == REPAIR), aes(x = mutator, y = entropy))
  g = g + geom_boxplot(aes(color = mutator), alpha = 0.5)
  g = g + theme_bw()
  g = g + scale_color_brewer(palette = "Dark2")
  g = g + theme(legend.position = "top")
  g = g + guides(colour = guide_legend(nrow = 1))
  g = g + labs(
    #title = "Distribution of population diversity for different mutators",
    #subtitle = sprintf("Repair mechanism: %s", REPAIR),
    ylab = "Entropy",
    xlab = "Mutator",
    color = "Mutator")
  #g = g + facet_grid(generator + D ~ eps + mu, scales = "free_y")
  g = g + facet_wrap(. ~ facet_var, scales = "free_y", ncol = 6L)
  fn = file.path(image_dir, sprintf("boxplots_mutator_entropy_%s.pdf", REPAIR))
  ggsave(fn, width = 10, height = 10)
}

tbl2 = tbl
#tbl2$entropy = log(tbl2$entropy)
g = ggplot(tbl2, aes(x = interaction(repair, mutator, sep = " / "), y = entropy))
g = g + geom_boxplot(aes(color = mutator), alpha = 0.5)
#g = g + scale_y_log10()
g = g + ylim(c(25, 38))
g = g + theme_bw()
g = g + scale_color_brewer(palette = "Dark2")
g = g + theme(legend.position = "top", axis.text.x = element_text(hjust = 1, angle = 45))
g = g + guides(colour = guide_legend(nrow = 1))
g = g + labs(
  #title = "Distribution of population diversity for different mutators",
  #subtitle = sprintf("Repair mechanism: %s", REPAIR),
  ylab = "Entropy",
  xlab = "Mutator / Repair",
  color = "Mutator")
#g = g + facet_grid(generator + D ~ eps + mu, scales = "free_y")
g = g + facet_wrap(. ~ facet_var, scales = "free_y", ncol = 3L)
fn = file.path(image_dir, "boxplots_mutator_entropy_all_repairs.pdf")
ggsave(fn, plot = g, width = 10, height = 12)

#stop("")

# AGGREGATED RANKING
# ===
aggr = tbl %>%
  group_by(generator, mutator, D, eps, mu, repair) %>%
  dplyr::summarize(entropy_mean = mean(entropy)) %>%
  ungroup() %>%
  group_by(generator, D, eps, mu, repair) %>%
  dplyr::mutate(rank = rank(entropy_mean)) %>%
  ungroup()

table(aggr$mutator, aggr$rank, aggr$repair)


# EFFECT OF REPAIR
# ===
tbl2 = tbl %>% group_by(repair, mutator) %>%
  dplyr::summarise(
    no_improve = round(mean(iters_without_improvement / iters) * 100, digits = 2),
    did_repair = round(mean(repair_operations / (iters)) * 100, digits = 2)) %>%
  arrange(no_improve) %>%
  ungroup() %>%
  reshape2::melt(id.vars = c("repair", "mutator"), variable.name = "split", value.name = "value")

g = ggplot(tbl2, aes(x = mutator, y = value, fill = split))
g = g + geom_bar(stat = "identity", position = "dodge", alpha = 0.5)
g = g + theme_bw()
g = g + scale_fill_brewer(palette = "Dark2")
#g = g + theme(legend.position = "top", axis.text.x = element_text(hjust = 1, angle = 45))
g = g + facet_grid(. ~ repair)
#print(g)


loadTraces = function(tbl, step = 100) {
  res = lapply(seq_len(nrow(tbl)), function(i) {
    worktbl = tbl[i, ]
    fn = file.path(out.dir.local, sprintf("%i.out", worktbl$jobid))
    #print(fn)
    trace = read.table(
      file = fn,
      header = FALSE,
      nrow = length(readLines(fn)) - 1L)
    colnames(trace) = c("iter", "diversity", "improved", "infeasible", "repaired")
    worktbl = cbind(trace, worktbl)
    filter(worktbl, ((iter %% step) == 0) | iter == 1)
  })
  return(dplyr::as_tibble(do.call(rbind, res)))
}

# SAMPLE TRAJECTORIES
# ===

traces = tbl %>%
  filter(mu == 50) %>% #a, generator %in% c("uncorr", "scorr", "invscorr")) %>%
  loadTraces()

traces2 = traces %>%
  group_by(eps, D, mu, repair, generator, iter, facet_var, mutator) %>%
  dplyr::summarize(diversity = mean(diversity))


for (REPAIR in unique(tbl$repair)) {
  g = ggplot(filter(traces2, repair == REPAIR), aes(x = iter, y = diversity, shape = mutator, color = mutator, group = mutator))
  g = g + geom_path(alpha = 0.5) + geom_point(aplha = 0.5, size = 0.8)
  g = g + scale_color_brewer(palette = "Dark2")
  g = g + xlim(c(0, 2500))
  g = g + theme_bw()
  g = g + theme(legend.position = "top")
  g = g + labs(
    #title = "Aggregated Trajectories",
    #subtitle = sprintf("Repair mechanism: %s", REPAIR),
    ylab = "Entropy",
    xlab = "Iteration",
    color = "Mutator",
    shape = "Mutator")
  g = g + guides(colour = guide_legend(nrow = 1))
  g = g + facet_wrap(. ~ facet_var, ncol = 3)
  #print(g)
  fn = file.path(image_dir, sprintf("traces_%s.pdf", REPAIR))
  ggsave(fn, plot = g, width = 8, height = 10)
}
